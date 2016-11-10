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
module StringMap = Map.Make(String)
type fragment_map = Parser.fragment StringMap.t

let rec collect_fields : fragment_map -> ('ctx, 'src) Schema.obj -> Parser.selection list -> Parser.field list = fun fragment_map obj fields -> 
  List.map (function
  | Parser.Field field ->
      [field]
  | Parser.FragmentSpread spread ->
      (try
        let fragment = StringMap.find spread.name fragment_map in
        if obj.name = fragment.type_condition then
          collect_fields fragment_map obj fragment.selection_set
        else
          []
      with Not_found -> [])
  | Parser.InlineFragment fragment ->
      match fragment.type_condition with
      | None ->
          collect_fields fragment_map obj fragment.selection_set
      | Some condition ->
          if condition = obj.name then
            collect_fields fragment_map obj fragment.selection_set
          else
            []
  ) fields |> List.concat

let alias_or_name (field : Parser.field) =
  match field.alias with
  | Some alias -> alias
  | None       -> field.name

let field_from_object : ('ctx, 'src) Schema.obj -> string -> ('ctx, 'src) Schema.field option = fun obj field_name ->
  try
    let field = List.find (fun (Schema.Field field) -> field.name = field_name) obj.fields in 
    Some field
  with Not_found -> None

let option_map opt default f =
  match opt with
  | None -> default
  | Some x -> f x

let rec present : type src. 'ctx -> src -> fragment_map -> Parser.field -> ('ctx, src) Schema.typ -> Yojson.Basic.json = fun ctx src fragment_map query_field typ ->
  match typ with
  | Schema.Scalar s -> option_map src `Null s.coerce
  | Schema.List t ->
      option_map src `Null (fun src' ->
        `List (List.map (fun x -> present ctx x fragment_map query_field t) src'))
  | Schema.NonNullable t -> present ctx (Some src) fragment_map query_field t
  | Schema.Object o ->
      option_map src `Null (fun src' ->
        let fields = collect_fields fragment_map o query_field.selection_set in
        `Assoc (resolve_fields ctx src' fragment_map o fields))
  | Schema.Enum e ->
      option_map src `Null (fun src' ->
        try
          let _, s = List.find (fun (v, s) -> src' == v) e.values in
          `String s
        with Not_found -> `Null)

and resolve_field : type src. 'ctx -> src -> fragment_map -> Parser.field -> ('ctx, src) Schema.field -> (string * Yojson.Basic.json) = fun ctx src fragment_map query_field (Schema.Field field) ->
  let name     = alias_or_name query_field in
  let resolved = field.resolve ctx src in
  let value    = present ctx resolved fragment_map query_field field.typ in
  (name, value)

and resolve_fields : type src. 'ctx -> src -> fragment_map -> ('ctx, src) Schema.obj -> Parser.field list -> (string * Yojson.Basic.json) list = fun ctx src fragment_map obj fields ->
  List.map (fun (query_field : Parser.field) ->
    match field_from_object obj query_field.name with
    | Some field -> resolve_field ctx src fragment_map query_field field
    | None       -> (alias_or_name query_field, `Null)
  ) fields

let execute_operation (schema : 'ctx Schema.schema) fragment_map (operation : Parser.operation) ctx =
  match operation.optype with
  | Parser.Query ->
      let query  = schema.query in
      let fields = collect_fields fragment_map query operation.selection_set in
      let props  = resolve_fields ctx () fragment_map query fields in
      (`Assoc props)
  | Parser.Mutation ->
      `String "Mutation is not implemented"
  | Parser.Subscription ->
      `String "Subscription is not implemented"

let collect_fragments doc =
  List.fold_left (fun memo -> function
    | Parser.Operation _ -> memo
    | Parser.Fragment f -> StringMap.add f.name f memo
  ) StringMap.empty doc

let select_operation doc =
  try
    let Parser.Operation op = List.find (function
      | Parser.Operation _ -> true
      | Parser.Fragment _ -> false
    ) doc in
    Some op
  with Not_found -> None

let execute schema ctx doc =
  let fragment_map = collect_fragments doc in
  let schema' = Introspection.add_schema_field schema in
  match select_operation doc with
  | None -> `String "No operation found"
  | Some op -> execute_operation schema' fragment_map op ctx
