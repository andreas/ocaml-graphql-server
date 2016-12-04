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

module Schema = struct
  (* Schema data types *)
  type 'a scalar = {
    name    : string;
    coerce : 'a -> Yojson.Basic.json;
  }

  type 'a enum = {
    name    : string;
    values  : ('a * string) list;
  }

  type 'a arg = {
    name   : string;
    coerce : string -> ('a, string) result
  }

  type ('ctx, 'src) obj = {
    name   : string;
    fields : ('ctx, 'src) field list;
  }
  and ('ctx, 'src) field =
    Field : {
      name    : string;
      typ     : ('ctx, 'a) typ;
      resolve : 'ctx -> 'src -> 'a;
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
  let field ~name ~typ ~resolve = Field { name; typ; resolve }
  let enum ~name ~values = Enum { name; values }
  let scalar ~name ~coerce = Scalar { name; coerce }
  let list typ = List typ
  let non_null typ = NonNullable typ

  (* Built-in scalars *)
  let int : 'ctx. ('ctx, int option) typ = Scalar {
    name   = "int";
    coerce = fun i -> `Int i;
  }

  let string : 'ctx. ('ctx, string option) typ = Scalar {
    name   = "string";
    coerce = fun s ->`String s;
  }

  let bool : 'ctx. ('ctx, bool option) typ = Scalar {
    name = "boolean";
    coerce = fun b -> `Bool b;
  }

  let float : 'ctx. ('ctx, float option) typ = Scalar {
    name = "float";
    coerce = fun f -> `Float f;
  }
end

module Introspection = struct
  open Schema

  (* ityp hides type parameters to avoid scope escaping errors *)
  type ityp = ITyp : ('ctx, 'src) typ -> ityp

  (* ifield hides the type parameters to avoid scope escaping errors *)
  type ifield = IField : ('ctx, 'src) field -> ifield

  (* Extracts all types contained in a single type *)
  let rec types : type src. ityp list -> ('ctx, src) typ -> ityp list = fun memo typ -> match typ with
    | List typ -> types memo typ
    | NonNullable typ -> types memo typ
    | Scalar _ as scalar -> (ITyp scalar)::memo 
    | Enum _ as enum -> (ITyp enum)::memo
    | Object o as obj ->
        let memo'   = ((ITyp obj)::memo) in
        let reducer = fun memo (Field f) -> types memo f.typ in
        List.fold_left reducer memo' o.fields 

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
        name = "string";
        typ = NonNullable string;
        resolve = fun _ name -> name
      };
      Field {
        name = "description";
        typ = string;
        resolve = fun _ e -> None
      };
      Field {
        name = "isDeprecated";
        typ = NonNullable bool;
        resolve = fun _ e -> false
      };
      Field {
        name = "deprecationReason";
        typ = string;
        resolve = fun _ e -> None
      }
    ]
  }

  let __input_value = Object {
    name = "__InputValue";
    fields = []
  }

  let rec __type : 'ctx. ('ctx, ityp option) typ = Object {
    name = "__Type";
    fields = [
      Field {
        name = "kind";
        typ = NonNullable __type_kind;
        resolve = fun _ (ITyp t) -> match t with
          | Object _  -> `Object
          | List _    -> `List
          | Scalar _  -> `Scalar
          | Enum _    -> `Enum
          | NonNullable _ -> `NonNull
      };
      Field {
        name = "name";
        typ = string;
        resolve = fun _ (ITyp t) -> match t with
          | Object o -> Some o.name
          | Scalar s -> Some s.name
          | Enum e -> Some e.name
          | _ -> None;
      };
      Field {
        name = "description";
        typ = string;
        resolve = fun _ t -> None;
      };
      Field {
        name = "fields";
        typ = List (NonNullable __field);
        resolve = fun _ (ITyp t) -> match t with
          | Object o -> Some (List.map (fun f -> IField f) o.fields)
          | _ -> None
      };
      Field {
        name = "interfaces";
        typ = List __type;
        resolve = fun _ (ITyp t) -> match t with
          | Object _ -> Some []
          | _ -> None
      };
      Field {
        name = "possibleTypes";
        typ = List __type;
        resolve = fun _ t -> None
      };
      Field {
        name = "ofType";
        typ = __type;
        resolve = fun _ (ITyp t) -> match t with
          | NonNullable typ -> Some (ITyp typ)
          | List typ     -> Some (ITyp typ)
          | _        -> None
      };
      Field {
        name = "enumValues";
        typ = List (NonNullable __enum_value);
        resolve = fun _ (ITyp t) -> match t with
          | Enum e -> Some (List.map snd e.values)
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
        resolve = fun _ (IField (Field f)) -> f.name
      };
      Field {
        name = "description";
        typ = string;
        resolve = fun _ f -> None
      };
      Field {
        name = "args";
        typ = NonNullable (List __input_value);
        resolve = fun _ f -> [];
      };
      Field {
        name = "type";
        typ = NonNullable __type;
        resolve = fun _ (IField (Field f)) -> ITyp f.typ
      };
      Field {
        name = "isDeprecated";
        typ = NonNullable bool;
        resolve = fun _ f -> false
      };
      Field {
        name = "deprecationReason";
        typ = string;
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
        resolve = fun _ s -> types [] (Object s.query);
      };
      Field {
        name = "queryType";
        typ = NonNullable __type;
        resolve = fun _ s -> ITyp (Object s.query);
      };
      Field {
        name = "mutationType";
        typ = __type;
        resolve = fun _ s -> None;
      };
      Field {
        name = "directives";
        typ = NonNullable (List (NonNullable __directive));
        resolve = fun _ s -> []
      }
    ]
  }

  let add_schema_field s =
    let schema_field = Field {
      name = "__schema";
      typ = NonNullable __schema;
      resolve = fun _ _ -> s
    } in
    { query = { s.query with fields = schema_field::s.query.fields } }
end

(* Parsing *)
module Parser = Graphql_parser

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

and resolve_field : type src. 'ctx -> src -> fragment_map -> Parser.field -> ('ctx, src) Schema.field -> (string * Yojson.Basic.json, string) result = fun ctx src fragment_map query_field (Schema.Field field) ->
  let name     = alias_or_name query_field in
  let resolved = field.resolve ctx src in
  present ctx resolved fragment_map query_field field.typ >>| fun value' ->
  name, value'

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
