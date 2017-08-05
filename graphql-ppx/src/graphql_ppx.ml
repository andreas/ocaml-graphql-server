open Migrate_parsetree
open Ast_403

module StringMap = Map.Make(String)

type ctx = {
  fragments : Graphql_parser.fragment StringMap.t;
  types : Introspection.typ list;
}

let loc = !Ast_helper.default_loc

let lid s : Ast_helper.lid =
  { txt = Longident.Lident s; loc = !Ast_helper.default_loc }

let str txt : Ast_helper.str = { txt; loc = !Ast_helper.default_loc }

let const_string s =
  Ast_helper.Exp.constant (Pconst_string (s, None))

let failwithf format =
  Format.ksprintf failwith format

let rec exprs_to_list = function
  | [] ->
      Ast_helper.Exp.construct (lid "[]") None
  | expr::exprs ->
      Ast_helper.Exp.(construct (lid "::") (Some (tuple [expr; exprs_to_list exprs])))

let select_field typ field_name =
  let find_field type_name fields =
    try
      List.find (fun (field : Introspection.field) -> field.name = field_name) fields
    with Not_found ->
      failwithf "Invalid field `%s` for type `%s`" field_name type_name
  in
  match typ with
  | Introspection.Object o ->
      find_field o.name o.fields
  | Introspection.Interface i ->
      find_field i.name i.fields
  | Introspection.Union u ->
      failwith "Cannot select field from union"
  | Introspection.Enum _ ->
      failwith "Cannot select field from enum"
  | Introspection.Scalar _ ->
      failwith "Cannot select field from scalar"
  | Introspection.InputObject _ ->
      failwith "Cannot select field from input object"

let match_type_name type_name typ =
  match typ with
  | Introspection.Object o -> o.name = type_name
  | Introspection.Union u -> u.name = type_name
  | Introspection.Interface i -> i.name = type_name
  | Introspection.Enum e -> e.name = type_name
  | Introspection.Scalar s -> s.name = type_name
  | Introspection.InputObject io -> io.name = type_name

let rec collect_fields ctx type_name fields =
  List.map (function
    | Graphql_parser.Field field ->
        [field]
    | Graphql_parser.FragmentSpread spread ->
        begin try
          let fragment = StringMap.find spread.name ctx.fragments in
          if fragment.type_condition = type_name then
            collect_fields ctx type_name fragment.selection_set
          else
            []
        with Not_found ->
          failwithf "Invalid fragment `%s`" spread.name
        end
    | Graphql_parser.InlineFragment fragment ->
        match fragment.type_condition with
        | None ->
            collect_fields ctx type_name fragment.selection_set
        | Some condition when condition = type_name ->
            collect_fields ctx type_name fragment.selection_set
        | _ -> []
    ) fields
  |> List.concat

let alias_or_name : Graphql_parser.field -> string = fun field ->
  match field.alias with
  | Some alias -> alias
  | None -> field.name

let convert_scalar : string -> Parsetree.expression =
  function
  | "Int" -> [%expr to_int_option]
  | "Boolean" -> [%expr to_bool_option]
  | "String" -> [%expr to_string_option]
  | "Float" -> [%expr to_float_option]
  | "ID" -> [%expr function
    | `Null -> None
    | `String s -> Some s
    | `Int n -> Some (string_of_int n)
    | json -> raise (Yojson.Basic.Util.Type_error ("Invalid type for ID", json))
  ]
  | typ -> failwithf "Unknown scalar type `%s`" typ

let convert_enum enum_values =
  let default_case = Ast_helper.(Exp.case Pat.(any ()) [%expr failwith "Invalid enum value"]) in
  let cases = List.fold_left (fun memo (value : Introspection.enum_value) ->
      let pattern = Ast_helper.Pat.constant (Pconst_string (value.name, None)) in
      let expr = Ast_helper.Exp.variant value.name None in
      (Ast_helper.Exp.case pattern expr)::memo
    ) [default_case] enum_values in
  Ast_helper.Exp.function_ cases

let parse_json_method method_name expr =
  let val_ = Ast_helper.Cf.(val_ (str method_name) Asttypes.Immutable (concrete Asttypes.Fresh expr)) in
  let method_ = Ast_helper.(Cf.method_ (str method_name) Asttypes.Public (Cf.concrete Asttypes.Fresh (Exp.ident (lid method_name)))) in
  [val_; method_]

let rec resolve_type_ref : ctx -> Introspection.typ -> Graphql_parser.field -> Introspection.type_ref -> Parsetree.expression =
  fun ctx obj query_field type_ref ->
    match type_ref with
    | Introspection.Type type_name ->
        begin match List.find (match_type_name type_name) ctx.types with
          | Introspection.Scalar s ->
              let convert_expr = convert_scalar s.name in
              [%expr json |> [%e convert_expr]]
          | Introspection.Enum e ->
              let convert_expr = convert_enum e.enum_values in
              [%expr json |> to_option (fun json -> json |> to_string |> [%e convert_expr])]
          | Introspection.Object o as obj ->
              let fields = collect_fields ctx o.name query_field.selection_set in
              let methods = resolve_fields ctx obj fields in
              let convert_expr = Ast_helper.(Exp.object_ (Cstr.mk (Pat.any ()) methods)) in
              [%expr json |> to_option (fun json -> [%e convert_expr])]
          | Introspection.Interface _ ->
              failwithf "Interface not supported yet"
          | Introspection.Union _ ->
              failwithf "Union not supported yet"
          | Introspection.InputObject _ ->
              failwithf "Input object `%s` cannot be used in selection set" type_name
          | exception Not_found ->
              failwithf "Unknown type `%s`" type_name
        end
    | Introspection.NonNull t ->
        let expr = resolve_type_ref ctx obj query_field t in
        [%expr match [%e expr] with None -> failwith "NonNull field was null" | Some v -> v]
    | Introspection.List t ->
        let expr = resolve_type_ref ctx obj query_field t in
        [%expr json |> to_option (convert_each (fun json -> [%e expr]))]

and resolve_field : ctx -> Introspection.typ -> Graphql_parser.field -> Parsetree.class_field list =
  fun ctx typ query_field ->
    let field = select_field typ query_field.name in
    let alias = alias_or_name query_field in
    let parse_expr = resolve_type_ref ctx typ query_field field.typ in
    parse_json_method alias [%expr let json = member [%e const_string alias] json in [%e parse_expr]]

and resolve_fields ctx obj fields : Parsetree.class_field list =
  List.map (resolve_field ctx obj) fields
  |> List.concat

let generate_parse_fn : Introspection.schema -> Graphql_parser.document -> Parsetree.expression =
  fun schema [Graphql_parser.Operation op] ->
    let typ = List.find (match_type_name schema.query_type) schema.types in
    let ctx = { fragments = StringMap.empty; types = schema.types } in
    let fields = collect_fields ctx schema.query_type op.selection_set in
    let methods = resolve_fields ctx typ fields in
    [%expr fun json ->
      let open Yojson.Basic.Util in
      let json = member "data" json in
      [%e Ast_helper.(Exp.object_ (Cstr.mk (Pat.any ()) methods))]
    ]

let accept_none : Parsetree.expression -> (Parsetree.expression -> Parsetree.expression) -> Parsetree.expression =
  fun value expr_fn ->
    [%expr match [%e value] with
      | None -> `Null
      | Some x -> [%e expr_fn [%expr x]]
    ]

let scalar_to_yojson name value : Parsetree.expression =
  match name with
  | "Int" -> [%expr `Int [%e value]]
  | "Boolean" -> [%expr `Bool [%e value]]
  | "ID"
  | "String" -> [%expr `String  [%e value]]
  | "Float" -> [%expr `Float [%e value]]
  | typ -> failwithf "Unknown scalar type `%s`" typ

let enum_to_yojson enum_values value : Parsetree.expression =
  let cases = List.map (fun (value : Introspection.enum_value) ->
      let pattern = Ast_helper.Pat.variant value.name None in
      let expr : Parsetree.expression = [%expr `String [%e const_string value.name]] in
      Ast_helper.Exp.case pattern expr
    ) enum_values
  in
  Ast_helper.Exp.match_ value cases

let rec schema_typ_to_yojson ?(nullable=true) types typ value =
  let handle_none = if nullable then accept_none value else (fun expr_fn -> expr_fn value) in
  match typ with
  | Introspection.Type type_name ->
      handle_none begin match List.find (match_type_name type_name) types with
        | Introspection.Scalar s ->
            scalar_to_yojson s.name
        | Introspection.Enum e ->
            enum_to_yojson e.enum_values
        | Introspection.InputObject o ->
            failwith "Input objects are not supported yet"
        | Introspection.Object _
        | Introspection.Interface _
        | Introspection.Union _ ->
            failwithf "Invalid argument type `%s` (must be scalar, enum or input object)" type_name
        | exception Not_found ->
            failwithf "Unknown argument type `%s`" type_name
      end
  | Introspection.List typ' ->
      let expr = schema_typ_to_yojson types typ' [%expr x] in
      handle_none (fun value' -> [%expr `List (List.map (fun x -> [%e expr]) [%e value'])])
  | Introspection.NonNull typ' ->
      schema_typ_to_yojson ~nullable:false types typ' value

let rec input_typ_to_introspection_typ = function
  | Graphql_parser.NamedType type_name ->
      Introspection.Type type_name
  | Graphql_parser.ListType typ' ->
      Introspection.List (input_typ_to_introspection_typ typ')
  | Graphql_parser.NonNullType typ' ->
      Introspection.NonNull (input_typ_to_introspection_typ typ')

let generate_variable_fn : Introspection.schema -> Graphql_parser.document -> Parsetree.expression =
  fun schema [Graphql_parser.Operation op] ->
    let properties = List.fold_right (fun (arg : Graphql_parser.variable_definition) memo ->
        let txt = Longident.Lident arg.name in
        let var = Ast_helper.Exp.ident {txt; loc} in
        let introspection_typ = input_typ_to_introspection_typ arg.typ in
        let expr : Parsetree.expression = [%expr [%e schema_typ_to_yojson schema.types introspection_typ var]] in
        let prop : Parsetree.expression = [%expr ([%e const_string arg.name], [%e expr])] in
        prop::memo
      ) op.variable_definitions [] in
    let prop_expr_list = exprs_to_list properties in
    let fn_with_cont : Parsetree.expression = [%expr fun () -> k ((`Assoc [%e prop_expr_list]) : Yojson.Basic.json)] in
    let fn_with_args = List.fold_right (fun (arg : Graphql_parser.variable_definition) memo ->
        let label = match arg.typ with
          | NonNullType _ ->
              Asttypes.Labelled arg.name 
          | NamedType _
          | ListType _ ->
              Asttypes.Optional arg.name
        in
        Ast_helper.(Exp.fun_ label None (Pat.var {txt=arg.name; loc}) memo)
      ) op.variable_definitions fn_with_cont
    in [%expr fun k -> [%e fn_with_args]]

let generate (loc : Location.t) query =
  let schema_path = (Location.absolute_path loc.loc_start.pos_fname |> Filename.dirname) ^ "/schema.json" in
  let schema = Introspection.of_file schema_path in
  match Graphql_parser.parse query with
  | Error err ->
      let msg = Format.sprintf "Invalid GraphQL query: %s" err in
      raise (Location.Error (Location.error ~loc msg))
  | Ok doc ->
      try
        Ast_helper.with_default_loc loc (fun () ->
            let variable_fn = generate_variable_fn schema doc in
            let parse_fn = generate_parse_fn schema doc in
            query, variable_fn, parse_fn
          )
      with Failure msg -> raise (Location.Error (Location.error ~loc msg))

let mapper _config _cookies =
  let default_mapper = Ast_mapper.default_mapper in
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "graphql"; loc}, pstr)} ->
          begin match pstr with
            | PStr [{ pstr_desc =
                        Pstr_eval ({ pexp_loc  = loc;
                                     pexp_desc = Pexp_constant (Pconst_string (query, _))}, _)}] ->
                let query, variable_fn, parse_fn = generate loc query in
                Ast_helper.Exp.tuple [const_string query; variable_fn; parse_fn]
            | _ ->
                raise (Location.Error (
                    Location.error ~loc "[%graphql] accepts a string, e.g. [%graphql \"query { id }\"]"))
          end
      | other -> default_mapper.expr mapper other
  }

let () =
  Driver.register ~name:"graphql_ppx"
    Versions.ocaml_403
    mapper
