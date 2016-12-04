open Rresult

(* Helper modules *)
module List = struct
  include List
  let assoc_exn = assoc
  let assoc x ys = try Some (assoc_exn x ys) with Not_found -> None

  let find_exn = find
  let find cond xs = try Some (find_exn cond xs) with Not_found -> None

  module Result = struct
    let rec join ?memo:(memo=[]) = function
      | [] -> Ok (List.rev memo)
      | (Error _ as err)::_ -> err
      | (Ok x)::xs -> join ~memo:(x::memo) xs

    let map_join f xs =
      List.map f xs |> join
  end
end

(* Parsing *)
module Parser = Graphql_parser

(* Schema *)
module Schema = struct
  module Arg = struct
    type (_, _) arg_typ =
      | Scalar : {
          name   : string;
          coerce : Parser.value -> ('b, string) result;
        } -> ('a, 'b option -> 'a) arg_typ
      | Object : {
          name   : string;
          fields : ('a, 'b) arg_list;
          coerce : 'b;
        } -> ('c, 'a option -> 'c) arg_typ
      | Enum : {
          name   : string;
          values : (string * 'b) list;
        } -> ('a, 'b option -> 'a) arg_typ
      | List : ('a, 'b -> 'a) arg_typ -> ('a, 'b list option -> 'a) arg_typ
      | NonNullable : ('a, 'b option -> 'a) arg_typ -> ('a, 'b -> 'a) arg_typ
    and ('a, 'b) arg = {
      name : string;
      typ : ('a, 'b) arg_typ;
      default : 'a option;
    }
    and (_, _) arg_list =
      | [] : ('a, 'a) arg_list
      | (::) : ('b, 'c -> 'b) arg * ('a, 'b) arg_list -> ('a, 'c -> 'b) arg_list

    let arg ?default:(default=None) name ~typ = { name; typ; default }
    let scalar ~name ~coerce = Scalar { name; coerce }
    let enum ~name ~values = Enum { name; values }
    let obj ~name ~fields ~coerce = Object { name; fields; coerce }

    (* Built-in argument types *)
    let int = Scalar {
      name = "Int";
      coerce = function
        | Parser.Int n -> Ok n
        | _ -> Error "Invalid int"
    }

    let string = Scalar {
      name = "String";
      coerce = function
        | Parser.String s -> Ok s
        | _ -> Error "Invalid string"
    }

    let float = Scalar {
      name = "Float";
      coerce = function
        | Parser.Float f -> Ok f
        | Parser.Int n -> Ok (float_of_int n)
        | _ -> Error "Invalid float"
    }

    let bool = Scalar {
      name = "Boolean";
      coerce = function
        | Parser.Boolean b -> Ok b
        | _ -> Error "Invalid boolean"
    }

    let guid = Scalar {
      name = "ID";
      coerce = function
        | Parser.String s -> Ok s
        | Parser.Int n -> Ok (string_of_int n)
        | _ -> Error "Invalid ID"
    }

    let non_null typ = NonNullable typ
    let list typ = List typ

    let rec eval_arglist : type a b. (a, b) arg_list -> Parser.key_value list -> b -> (a, string) result =
      fun arglist key_values f ->
        match arglist with
        | [] -> Ok f
        | arg::arglist' ->
            let value = List.assoc arg.name key_values in
            eval_arg arg.typ value >>= fun coerced ->
            eval_arglist arglist' key_values (f coerced)

    and eval_arg : type a b. (a, b -> a) arg_typ -> Parser.value option -> (b, string) result = fun typ value ->
      match (typ, value) with
      | NonNullable _, None -> Error "Missing required argument"
      | NonNullable _, Some Parser.Null -> Error "Missing required argument"
      | Scalar _, None -> Ok None
      | Scalar _, Some Parser.Null -> Ok None
      | Object _, None -> Ok None
      | Object _, Some Parser.Null -> Ok None
      | List _, None -> Ok None
      | List _, Some Parser.Null -> Ok None
      | Enum _, None -> Ok None
      | Enum _, Some Parser.Null -> Ok None
      | Scalar s, Some value ->
          s.coerce value >>| fun coerced ->
          Some coerced
      | Object o, Some value ->
          begin match value with
          | Parser.Object key_values ->
              eval_arglist o.fields key_values o.coerce >>| fun coerced ->
              Some coerced
          | _ -> Error "Expected object"
          end
     | List typ, Some value ->
          begin match value with
          | Parser.List values ->
              let option_values = List.map (fun x -> Some x) values in
              List.Result.map_join (eval_arg typ) option_values >>| fun coerced ->
              Some coerced
          | value -> eval_arg typ (Some value) >>| fun coerced ->
              (Some [coerced] : b)
          end
      | NonNullable typ, value ->
          eval_arg typ value >>= (function
          | Some value -> Ok value
          | None -> Error "Missing required argument")
      | Enum e, Some value ->
          begin match value with
          | Parser.Enum v ->
              begin match List.assoc v e.values with
              | Some _ as value -> Ok value
              | None -> Error "Invalid enum value"
              end
          | _ -> Error "Expected enum"
          end
  end

  (* Schema data types *)
  type 'a scalar = {
    name    : string;
    coerce : 'a -> Yojson.Basic.json;
  }

  type 'a enum = {
    name    : string;
    values  : ('a * string) list;
  }

  type ('ctx, 'src) obj = {
    name   : string;
    fields : ('ctx, 'src) field list;
  }
  and ('ctx, 'src) field =
    Field : {
      name    : string;
      args    : ('a, 'b) Arg.arg_list;
      typ     : ('ctx, 'a) typ;
      resolve : 'ctx -> 'src -> 'b;
    } -> ('ctx, 'src) field
  and ('ctx, 'src) typ =
    | Object      : ('ctx, 'src) obj -> ('ctx, 'src option) typ
    | List        : ('ctx, 'src) typ -> ('ctx, 'src list option) typ
    | NonNullable : ('ctx, 'src option) typ -> ('ctx, 'src) typ
    | Scalar      : 'src scalar -> ('ctx, 'src option) typ
    | Enum        : 'src enum -> ('ctx, 'src option) typ

  type 'ctx schema = {
    query : ('ctx, unit) obj;
  }

  let schema ~fields = {
    query = {
      name = "root";
      fields;
    }
  }

  (* Constructor functions *)
  let obj ~name ~fields = Object { name; fields }
  let field ~name ~typ ~args ~resolve = Field { name; typ; args; resolve }
  let enum ~name ~values = Enum { name; values }
  let scalar ~name ~coerce = Scalar { name; coerce }
  let list typ = List typ
  let non_null typ = NonNullable typ

  (* Built-in scalars *)
  let int : 'ctx. ('ctx, int option) typ = Scalar {
    name   = "Int";
    coerce = fun i -> `Int i;
  }

  let string : 'ctx. ('ctx, string option) typ = Scalar {
    name   = "String";
    coerce = fun s ->`String s;
  }

  let bool : 'ctx. ('ctx, bool option) typ = Scalar {
    name = "Boolean";
    coerce = fun b -> `Bool b;
  }

  let float : 'ctx. ('ctx, float option) typ = Scalar {
    name = "Float";
    coerce = fun f -> `Float f;
  }

  let guid : 'ctx. ('ctx, string option) typ = Scalar {
    name = "ID";
    coerce = fun x -> `String x;
  }
end

module Introspection = struct
  open Schema

  (* ityp, ifield and iarg hide type parameters to avoid scope escaping errors *)
  type ityp =
    | ITyp : (_, _) typ -> ityp
    | IArgTyp : (_, _) Arg.arg_typ -> ityp
  type ifield =
    | IField : (_, _) field -> ifield
    | IArgField : (_, _) Arg.arg -> ifield
  type iarg = IArg : (_, _) Arg.arg -> iarg

  (* Extracts all types contained in a single type *)
  let rec types : type src. ityp list -> ('ctx, src) typ -> ityp list = fun memo typ -> match typ with
    | List typ -> types memo typ
    | NonNullable typ -> types memo typ
    | Scalar _ as scalar -> (ITyp scalar)::memo 
    | Enum _ as enum -> (ITyp enum)::memo
    | Object o as obj ->
        let memo'   = (ITyp obj)::memo in
        let reducer = fun memo (Field f) ->
          let memo' = types memo f.typ in
          arg_list_types memo' f.args
        in
        List.fold_left reducer memo' o.fields
  and arg_types : type a b. ityp list -> (a, b) Arg.arg_typ -> ityp list = fun memo argtyp ->
    match argtyp with
    | Arg.Scalar _ as scalar -> (IArgTyp scalar)::memo
    | Arg.Enum _ as enum -> (IArgTyp enum)::memo
    | Arg.List typ -> arg_types memo typ
    | Arg.NonNullable typ -> arg_types memo typ
    | Arg.Object o as obj ->
        let memo' = (IArgTyp obj)::memo in
        arg_list_types memo' o.fields
  and arg_list_types : type a b. ityp list -> (a, b) Arg.arg_list -> ityp list = fun memo arglist ->
    let open Arg in
    match arglist with
    | [] -> memo
    | arg::args ->
        let memo' = arg_types memo arg.typ in
        arg_list_types memo' args

  let rec args_to_list : type a b. ?memo:iarg list -> (a, b) Arg.arg_list -> iarg list = fun ?memo:(memo=[]) arglist ->
    let open Arg in
    match arglist with
    | [] ->
        memo
    | arg::args ->
        let memo' = List.cons (IArg arg) memo in
        args_to_list ~memo:memo' args
  ;;
  let __type_kind = Enum {
    name = "__TypeKind";
    values = [
      (`Scalar, "SCALAR");
      (`Object, "OBJECT");
      (`Interface, "INTERFACE");
      (`Union, "UNION");
      (`Enum, "ENUM");
      (`InputObject, "INPUT_OBJECT");
      (`List, "LIST");
      (`NonNull, "NON_NULL");
    ]
  }

  let __enum_value = Object {
    name = "__EnumValue";
    fields = [
      Field {
        name = "name";
        typ = NonNullable string;
        args = Arg.[];
        resolve = fun _ name -> name
      };
      Field {
        name = "description";
        typ = string;
        args = Arg.[];
        resolve = fun _ e -> None
      };
      Field {
        name = "isDeprecated";
        typ = NonNullable bool;
        args = Arg.[];
        resolve = fun _ e -> false
      };
      Field {
        name = "deprecationReason";
        typ = string;
        args = Arg.[];
        resolve = fun _ e -> None
      }
    ]
  }

  let rec __input_value : 'ctx. ('ctx, iarg option) typ = Object {
    name = "__InputValue";
    fields = [
      Field {
        name = "name";
        typ = NonNullable string;
        args = Arg.[];
        resolve = fun _ (IArg v) -> v.name
      };
      Field {
        name = "description";
        typ = string;
        args = Arg.[];
        resolve = fun _ _ -> None;
      };
      Field {
        name = "type";
        typ = NonNullable __type;
        args = Arg.[];
        resolve = fun _ (IArg v) -> IArgTyp v.typ;
      };
      Field {
        name = "defaultValue";
        typ = string;
        args = Arg.[];
        resolve = fun _ v -> None;
      }
    ]
  }

  and __type : 'ctx. ('ctx, ityp option) typ = Object {
    name = "__Type";
    fields = [
      Field {
        name = "kind";
        typ = NonNullable __type_kind;
        args = Arg.[];
        resolve = fun _ t -> match t with
          | ITyp (Object _) -> `Object
          | ITyp (List _) -> `List
          | ITyp (Scalar _) -> `Scalar
          | ITyp (Enum _) -> `Enum
          | ITyp (NonNullable _) -> `NonNull
          | IArgTyp (Arg.Object _) -> `InputObject
          | IArgTyp (Arg.List _) -> `List
          | IArgTyp (Arg.Scalar _) -> `Scalar
          | IArgTyp (Arg.Enum _) -> `Enum
          | IArgTyp (Arg.NonNullable _) -> `NonNull
      };
      Field {
        name = "name";
        typ = string;
        args = Arg.[];
        resolve = fun _ t -> match t with
          | ITyp (Object o) -> Some o.name
          | ITyp (Scalar s) -> Some s.name
          | ITyp (Enum e) -> Some e.name
          | IArgTyp (Arg.Object o) -> Some o.name;
          | IArgTyp (Arg.Scalar s) -> Some s.name;
          | IArgTyp (Arg.Enum e) -> Some e.name
          | _ -> None;
      };
      Field {
        name = "description";
        typ = string;
        args = Arg.[];
        resolve = fun _ t -> None;
      };
      Field {
        name = "fields";
        typ = List (NonNullable __field);
        args = Arg.[];
        resolve = fun _ t -> match t with
          | ITyp (Object o) ->
              Some (List.map (fun f -> IField f) o.fields)
          | IArgTyp (Arg.Object o) ->
              let arg_list = args_to_list o.fields in
              Some (List.map (fun (IArg f) -> IArgField f) arg_list)
          | _ -> None
      };
      Field {
        name = "interfaces";
        typ = List __type;
        args = Arg.[];
        resolve = fun _ t -> match t with
          | ITyp (Object _) -> Some []
          | _ -> None
      };
      Field {
        name = "possibleTypes";
        typ = List __type;
        args = Arg.[];
        resolve = fun _ t -> None
      };
      Field {
        name = "ofType";
        typ = __type;
        args = Arg.[];
        resolve = fun _ t -> match t with
          | ITyp (NonNullable typ) -> Some (ITyp typ)
          | ITyp (List typ) -> Some (ITyp typ)
          | IArgTyp (Arg.NonNullable typ) -> Some (IArgTyp typ)
          | IArgTyp (Arg.List typ) -> Some (IArgTyp typ)
          | _ -> None
      };
      Field {
        name = "inputFields";
        typ = List (NonNullable __input_value);
        args = Arg.[];
        resolve = fun _ t -> match t with
          | IArgTyp (Arg.Object o) ->
              Some (args_to_list o.fields)
          | _ -> None
      };
      Field {
        name = "enumValues";
        typ = List (NonNullable __enum_value);
        args = Arg.[];
        resolve = fun _ t -> match t with
          | ITyp (Enum e) -> Some (List.map snd e.values)
          | IArgTyp (Arg.Enum e) -> Some (List.map fst e.values)
          | _      -> None
      }
    ]
  }

  and __field : 'ctx. ('ctx, ifield option) typ = Object {
    name = "__Field";
    fields = [
      Field {
        name = "name";
        typ = NonNullable string;
        args = Arg.[];
        resolve = fun _ f -> match f with
          | IField (Field f) -> f.name
          | IArgField a -> a.name
      };
      Field {
        name = "description";
        typ = string;
        args = Arg.[];
        resolve = fun _ f -> None
      };
      Field {
        name = "args";
        typ = NonNullable (List (NonNullable __input_value));
        args = Arg.[];
        resolve = fun _ f -> match f with
          | IField (Field f) -> args_to_list f.args
          | IArgField _ -> []
      };
      Field {
        name = "type";
        typ = NonNullable __type;
        args = Arg.[];
        resolve = fun _ f -> match f with
          | IField (Field f) -> ITyp f.typ
          | IArgField a -> IArgTyp a.typ
      };
      Field {
        name = "isDeprecated";
        typ = NonNullable bool;
        args = Arg.[];
        resolve = fun _ f -> false
      };
      Field {
        name = "deprecationReason";
        typ = string;
        args = Arg.[];
        resolve = fun _ f -> None
      }
    ]
  }

  let __directive = Object {
    name = "__Directive";
    fields = [
      Field {
        name = "name";
        typ = NonNullable string;
        args = Arg.[];
        resolve = fun _ d -> d.name
      }
    ]
  }

  let __schema : 'ctx. ('ctx, 'ctx schema option) typ = Object {
    name = "__Schema";
    fields = [
      Field {
        name = "types";
        typ = NonNullable (List (NonNullable __type));
        args = Arg.[];
        resolve = fun _ s -> types [] (Object s.query);
      };
      Field {
        name = "queryType";
        typ = NonNullable __type;
        args = Arg.[];
        resolve = fun _ s -> ITyp (Object s.query);
      };
      Field {
        name = "mutationType";
        typ = __type;
        args = Arg.[];
        resolve = fun _ s -> None;
      };
      Field {
        name = "directives";
        typ = NonNullable (List (NonNullable __directive));
        args = Arg.[];
        resolve = fun _ s -> []
      }
    ]
  }

  let add_schema_field s =
    let schema_field = Field {
      name = "__schema";
      typ = NonNullable __schema;
      args = Arg.[];
      resolve = fun _ _ -> s
    } in
    { query = { s.query with fields = schema_field::s.query.fields } }
end

(* Execution *)
module StringMap = struct
  include Map.Make(String)
  let find_exn = find
  let find k t = try Some(find_exn k t) with Not_found -> None
end
type fragment_map = Parser.fragment StringMap.t

let rec collect_fields : fragment_map -> ('ctx, 'src) Schema.obj -> Parser.selection list -> Parser.field list = fun fragment_map obj fields -> 
  List.map (function
  | Parser.Field field ->
      [field]
  | Parser.FragmentSpread spread ->
      begin match StringMap.find spread.name fragment_map with
      | Some fragment when obj.name = fragment.type_condition ->
          collect_fields fragment_map obj fragment.selection_set
      | _ ->
          []
      end
  | Parser.InlineFragment fragment ->
      match fragment.type_condition with
      | None ->
          collect_fields fragment_map obj fragment.selection_set
      | Some condition when condition = obj.name ->
          collect_fields fragment_map obj fragment.selection_set
      | _ -> []
  ) fields |> List.concat

let alias_or_name : Parser.field -> string = fun field ->
  match field.alias with
  | Some alias -> alias
  | None       -> field.name

let field_from_object : ('ctx, 'src) Schema.obj -> string -> ('ctx, 'src) Schema.field option = fun obj field_name ->
  List.find (fun (Schema.Field field) -> field.name = field_name) obj.fields

let coerce_or_null : 'a option -> ('a -> (Yojson.Basic.json, string) result) -> (Yojson.Basic.json, string) result = fun src f ->
  match src with
  | None -> Ok `Null
  | Some src' -> f src'

let rec present : type src. 'ctx -> src -> fragment_map -> Parser.field -> ('ctx, src) Schema.typ -> (Yojson.Basic.json, string) result = fun ctx src fragment_map query_field typ ->
  match typ with
  | Schema.Scalar s -> coerce_or_null src (fun x -> Ok (s.coerce x))
  | Schema.List t ->
      coerce_or_null src (fun src' ->
        List.Result.map_join (fun x ->
          present ctx x fragment_map query_field t
        ) src' >>| fun field_values ->
        `List field_values
      )
  | Schema.NonNullable t -> present ctx (Some src) fragment_map query_field t
  | Schema.Object o ->
      coerce_or_null src (fun src' ->
        let fields = collect_fields fragment_map o query_field.selection_set in
        resolve_fields ctx src' fragment_map o fields >>| fun field_values ->
        `Assoc field_values)
  | Schema.Enum e ->
      coerce_or_null src (fun src' ->
        match List.find (fun (v, s) -> src' == v) e.values with
        | Some (_, s) -> Ok (`String s)
        | None -> Ok `Null
      )

and resolve_field : type src. 'ctx -> src -> fragment_map -> Parser.field -> ('ctx, src) Schema.field -> ((string * Yojson.Basic.json), string) result = fun ctx src fragment_map query_field (Schema.Field field) ->
  let name     = alias_or_name query_field in
  let resolver = field.resolve ctx src in
  Schema.Arg.eval_arglist field.args query_field.arguments resolver >>= fun resolved ->
  present ctx resolved fragment_map query_field field.typ >>| fun value ->
  name, value

and resolve_fields : type src. 'ctx -> src -> fragment_map -> ('ctx, src) Schema.obj -> Parser.field list -> ((string * Yojson.Basic.json) list, string) result = fun ctx src fragment_map obj fields ->
  List.Result.map_join (fun (query_field : Parser.field) ->
    match field_from_object obj query_field.name with
    | Some field ->
        resolve_field ctx src fragment_map query_field field
    | None ->
        Ok (alias_or_name query_field, `Null)
  ) fields

let execute_operation : 'ctx Schema.schema -> fragment_map -> Parser.operation -> 'ctx -> (Yojson.Basic.json, string) result = fun schema fragment_map operation ctx ->
  match operation.optype with
  | Parser.Query ->
      let query  = schema.query in
      let fields = collect_fields fragment_map query operation.selection_set in
      resolve_fields ctx () fragment_map query fields >>| fun props ->
      (`Assoc props)
  | Parser.Mutation ->
      Error "Mutation is not implemented"
  | Parser.Subscription ->
      Error "Subscription is not implemented"

let collect_fragments doc =
  List.fold_left (fun memo -> function
    | Parser.Operation _ -> memo
    | Parser.Fragment f -> StringMap.add f.name f memo
  ) StringMap.empty doc

let rec select_operation = function
  | [] -> Error "No operation found"
  | (Parser.Operation op)::defs -> Ok op
  | _::defs -> select_operation defs

let execute schema ctx doc =
  let execute' schema ctx doc =
    let fragment_map = collect_fragments doc in
    let schema' = Introspection.add_schema_field schema in
    select_operation doc >>= fun op ->
    execute_operation schema' fragment_map op ctx
  in
  match execute' schema ctx doc with
  | Ok data   -> `Assoc ["data", data]
  | Error err -> `Assoc ["errors", `List [`Assoc ["message", `String err]]]
