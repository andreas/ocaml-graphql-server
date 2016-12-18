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

(* IO *)
module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Schema *)
module Make(Io : IO) = struct
  type +'a io = 'a Io.t

  module Io = struct
    include Io

    let map x ~f = bind x (fun x' -> return (f x'))
    let ok x = Io.return (Ok x)
    let error x = Io.return (Error x)

    let rec all ?memo:(memo=[]) = function
      | [] -> Io.return []
      | x::xs ->
          bind (all xs) (fun xs' ->
            map x ~f:(fun x' -> x'::xs')
          )

    module Result = struct
      let return x = return (Ok x)
      let bind x f = bind x (function Ok x' -> f x' | Error _ as err -> Io.return err)
      let map x ~f = map x ~f:(function Ok x' -> Ok (f x') | Error _ as err -> err)
    end

    module Infix = struct
      let (>>=) = bind
      let (>>|) = map
      let (>>=?) = Result.bind
      let (>>|?) = Result.map
    end
  end

  module Arg = struct
    open Rresult

    type (_, _) arg_typ =
      | Scalar : {
          name   : string;
          coerce : Graphql_parser.value -> ('b, string) result;
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

    let arg ?default:(default=None) name ~typ =
      { name; typ; default }

    let scalar ~name ~coerce =
      Scalar { name; coerce }

    let enum ~name ~values =
      Enum { name; values }

    let obj ~name ~fields ~coerce =
      Object { name; fields; coerce }

    (* Built-in argument types *)
    let int = Scalar {
      name = "Int";
      coerce = function
        | Graphql_parser.Int n -> Ok n
        | _ -> Error "Invalid int"
    }

    let string = Scalar {
      name = "String";
      coerce = function
        | Graphql_parser.String s -> Ok s
        | _ -> Error "Invalid string"
    }

    let float = Scalar {
      name = "Float";
      coerce = function
        | Graphql_parser.Float f -> Ok f
        | Graphql_parser.Int n -> Ok (float_of_int n)
        | _ -> Error "Invalid float"
    }

    let bool = Scalar {
      name = "Boolean";
      coerce = function
        | Graphql_parser.Boolean b -> Ok b
        | _ -> Error "Invalid boolean"
    }

    let guid = Scalar {
      name = "ID";
      coerce = function
        | Graphql_parser.String s -> Ok s
        | Graphql_parser.Int n -> Ok (string_of_int n)
        | _ -> Error "Invalid ID"
    }

    let non_null typ = NonNullable typ
    let list typ = List typ

    let rec eval_arglist : type a b. (a, b) arg_list -> Graphql_parser.key_value list -> b -> (a, string) result =
      fun arglist key_values f ->
        match arglist with
        | [] -> Ok f
        | arg::arglist' ->
            let value = List.assoc arg.name key_values in
            eval_arg arg.typ value >>= fun coerced ->
            eval_arglist arglist' key_values (f coerced)

    and eval_arg : type a b. (a, b -> a) arg_typ -> Graphql_parser.value option -> (b, string) result = fun typ value ->
      match (typ, value) with
      | NonNullable _, None -> Error "Missing required argument"
      | NonNullable _, Some Graphql_parser.Null -> Error "Missing required argument"
      | Scalar _, None -> Ok None
      | Scalar _, Some Graphql_parser.Null -> Ok None
      | Object _, None -> Ok None
      | Object _, Some Graphql_parser.Null -> Ok None
      | List _, None -> Ok None
      | List _, Some Graphql_parser.Null -> Ok None
      | Enum _, None -> Ok None
      | Enum _, Some Graphql_parser.Null -> Ok None
      | Scalar s, Some value ->
          s.coerce value >>| fun coerced ->
          Some coerced
      | Object o, Some value ->
          begin match value with
          | Graphql_parser.Object key_values ->
              eval_arglist o.fields key_values o.coerce >>| fun coerced ->
              Some coerced
          | _ -> Error "Expected object"
          end
     | List typ, Some value ->
          begin match value with
          | Graphql_parser.List values ->
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
          | Graphql_parser.Enum v ->
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
  and (_, _) field =
    Field : {
      name    : string;
      typ     : ('ctx, 'io_out) typ;
      args    : ('maybe_io_out, 'args) Arg.arg_list;
      resolve : 'ctx -> 'src -> 'args;
      lift    : 'maybe_io_out -> 'io_out Io.t
    } -> ('ctx, 'src) field
  and (_, _) typ =
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

  let id : 'a. 'a -> 'a = fun x -> x

  (* Constructor functions *)
  let obj ~name ~fields =
    Object { name; fields }

  let field name ~typ ~args ~resolve =
    Field { lift = Io.return; name; typ; args; resolve }

  let io_field name ~typ ~args ~resolve =
    Field { lift = id; name; typ; args; resolve }

  let enum ~name ~values =
    Enum { name; values }

  let scalar ~name ~coerce =
    Scalar { name; coerce }

  let list typ =
    List typ

  let non_null typ =
    NonNullable typ

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

module Introspection = struct
  (* any_typ, any_field and any_arg hide type parameters to avoid scope escaping errors *)
  type any_typ =
    | AnyTyp : (_, _) typ -> any_typ
    | AnyArgTyp : (_, _) Arg.arg_typ -> any_typ
  type any_field =
    | AnyField : (_, _) field -> any_field
    | AnyArgField : (_, _) Arg.arg -> any_field
  type any_arg = AnyArg : (_, _) Arg.arg -> any_arg

  (* Extracts all types contained in a single type *)
  let rec types : type src. any_typ list -> ('ctx, src) typ -> any_typ list = fun memo typ -> match typ with
    | List typ -> types memo typ
    | NonNullable typ -> types memo typ
    | Scalar _ as scalar -> (AnyTyp scalar)::memo
    | Enum _ as enum -> (AnyTyp enum)::memo
    | Object o as obj ->
        let memo'   = (AnyTyp obj)::memo in
        let reducer = fun memo (Field f) ->
          let memo' = types memo f.typ in
          arg_list_types memo' f.args
        in
        List.fold_left reducer memo' o.fields
  and arg_types : type a b. any_typ list -> (a, b) Arg.arg_typ -> any_typ list = fun memo argtyp ->
    match argtyp with
    | Arg.Scalar _ as scalar -> (AnyArgTyp scalar)::memo
    | Arg.Enum _ as enum -> (AnyArgTyp enum)::memo
    | Arg.List typ -> arg_types memo typ
    | Arg.NonNullable typ -> arg_types memo typ
    | Arg.Object o as obj ->
        let memo' = (AnyArgTyp obj)::memo in
        arg_list_types memo' o.fields
  and arg_list_types : type a b. any_typ list -> (a, b) Arg.arg_list -> any_typ list = fun memo arglist ->
    let open Arg in
    match arglist with
    | [] -> memo
    | arg::args ->
        let memo' = arg_types memo arg.typ in
        arg_list_types memo' args

  let rec args_to_list : type a b. ?memo:any_arg list -> (a, b) Arg.arg_list -> any_arg list = fun ?memo:(memo=[]) arglist ->
    let open Arg in
    match arglist with
    | [] ->
        memo
    | arg::args ->
        let memo' = List.cons (AnyArg arg) memo in
        args_to_list ~memo:memo' args

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
        lift = Io.return;
        resolve = fun _ name -> name;
      };
      Field {
        name = "description";
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ e -> None;
      };
      Field {
        name = "isDeprecated";
        typ = NonNullable bool;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ e -> false;
      };
      Field {
        name = "deprecationReason";
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ e -> None;
      }
    ]
  }

  let rec __input_value : 'ctx. ('ctx, any_arg option) typ = Object {
    name = "__InputValue";
    fields = [
      Field {
        name = "name";
        typ = NonNullable string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ (AnyArg v) -> v.name
      };
      Field {
        name = "description";
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ _ -> None;
      };
      Field {
        name = "type";
        typ = NonNullable __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ (AnyArg v) -> AnyArgTyp v.typ;
      };
      Field {
        name = "defaultValue";
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ v -> None;
      }
    ]
  }

  and __type : 'ctx. ('ctx, any_typ option) typ = Object {
    name = "__Type";
    fields = [
      Field {
        name = "kind";
        typ = NonNullable __type_kind;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (Object _) -> `Object
          | AnyTyp (List _) -> `List
          | AnyTyp (Scalar _) -> `Scalar
          | AnyTyp (Enum _) -> `Enum
          | AnyTyp (NonNullable _) -> `NonNull
          | AnyArgTyp (Arg.Object _) -> `InputObject
          | AnyArgTyp (Arg.List _) -> `List
          | AnyArgTyp (Arg.Scalar _) -> `Scalar
          | AnyArgTyp (Arg.Enum _) -> `Enum
          | AnyArgTyp (Arg.NonNullable _) -> `NonNull
      };
      Field {
        name = "name";
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (Object o) -> Some o.name
          | AnyTyp (Scalar s) -> Some s.name
          | AnyTyp (Enum e) -> Some e.name
          | AnyArgTyp (Arg.Object o) -> Some o.name;
          | AnyArgTyp (Arg.Scalar s) -> Some s.name;
          | AnyArgTyp (Arg.Enum e) -> Some e.name
          | _ -> None;
      };
      Field {
        name = "description";
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> None;
      };
      Field {
        name = "fields";
        typ = List (NonNullable __field);
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (Object o) ->
              Some (List.map (fun f -> AnyField f) o.fields)
          | AnyArgTyp (Arg.Object o) ->
              let arg_list = args_to_list o.fields in
              Some (List.map (fun (AnyArg f) -> AnyArgField f) arg_list)
          | _ -> None
      };
      Field {
        name = "interfaces";
        typ = List __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (Object _) -> Some []
          | _ -> None
      };
      Field {
        name = "possibleTypes";
        typ = List __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> None
      };
      Field {
        name = "ofType";
        typ = __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (NonNullable typ) -> Some (AnyTyp typ)
          | AnyTyp (List typ) -> Some (AnyTyp typ)
          | AnyArgTyp (Arg.NonNullable typ) -> Some (AnyArgTyp typ)
          | AnyArgTyp (Arg.List typ) -> Some (AnyArgTyp typ)
          | _ -> None
      };
      Field {
        name = "inputFields";
        typ = List (NonNullable __input_value);
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyArgTyp (Arg.Object o) ->
              Some (args_to_list o.fields)
          | _ -> None
      };
      Field {
        name = "enumValues";
        typ = List (NonNullable __enum_value);
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (Enum e) -> Some (List.map snd e.values)
          | AnyArgTyp (Arg.Enum e) -> Some (List.map fst e.values)
          | _      -> None
      }
    ]
  }

  and __field : 'ctx. ('ctx, any_field option) typ = Object {
    name = "__Field";
    fields = [
      Field {
        name = "name";
        typ = NonNullable string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> match f with
          | AnyField (Field f) -> f.name
          | AnyArgField a -> a.name
      };
      Field {
        name = "description";
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> None
      };
      Field {
        name = "args";
        typ = NonNullable (List (NonNullable __input_value));
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> match f with
          | AnyField (Field f) -> args_to_list f.args
          | AnyArgField _ -> []
      };
      Field {
        name = "type";
        typ = NonNullable __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> match f with
          | AnyField (Field f) -> AnyTyp f.typ
          | AnyArgField a -> AnyArgTyp a.typ
      };
      Field {
        name = "isDeprecated";
        typ = NonNullable bool;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> false
      };
      Field {
        name = "deprecationReason";
        typ = string;
        args = Arg.[];
        lift = Io.return;
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
        lift = Io.return;
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
        lift = Io.return;
        resolve = fun _ s -> types [] (Object s.query);
      };
      Field {
        name = "queryType";
        typ = NonNullable __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ s -> AnyTyp (Object s.query);
      };
      Field {
        name = "mutationType";
        typ = __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ s -> None;
      };
      Field {
        name = "directives";
        typ = NonNullable (List (NonNullable __directive));
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ s -> []
      }
    ]
  }

  let add_schema_field s =
    let schema_field = Field {
      name = "__schema";
      typ = NonNullable __schema;
      args = Arg.[];
      lift = Io.return;
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
  type fragment_map = Graphql_parser.fragment StringMap.t

  let rec collect_fields : fragment_map -> ('ctx, 'src) obj -> Graphql_parser.selection list -> Graphql_parser.field list = fun fragment_map obj fields -> 
    List.map (function
    | Graphql_parser.Field field ->
        [field]
    | Graphql_parser.FragmentSpread spread ->
        begin match StringMap.find spread.name fragment_map with
        | Some fragment when obj.name = fragment.type_condition ->
            collect_fields fragment_map obj fragment.selection_set
        | _ ->
            []
        end
    | Graphql_parser.InlineFragment fragment ->
        match fragment.type_condition with
        | None ->
            collect_fields fragment_map obj fragment.selection_set
        | Some condition when condition = obj.name ->
            collect_fields fragment_map obj fragment.selection_set
        | _ -> []
    ) fields
    |> List.concat

  let alias_or_name : Graphql_parser.field -> string = fun field ->
    match field.alias with
    | Some alias -> alias
    | None -> field.name

  let field_from_object : ('ctx, 'src) obj -> string -> ('ctx, 'src) field option = fun obj field_name ->
    List.find (fun (Field field) -> field.name = field_name) obj.fields

  let coerce_or_null : 'a option -> ('a -> (Yojson.Basic.json, string) result Io.t) -> (Yojson.Basic.json, string) result Io.t = fun src f ->
    match src with
    | None -> Io.ok `Null
    | Some src' -> f src'

  let rec present : type src. 'ctx -> src -> fragment_map -> Graphql_parser.field -> ('ctx, src) typ -> (Yojson.Basic.json, string) result Io.t = fun ctx src fragment_map query_field typ ->
    match typ with
    | Scalar s -> coerce_or_null src (fun x -> Io.return (Ok (s.coerce x)))
    | List t ->
        coerce_or_null src (fun src' ->
          List.map (fun x -> present ctx x fragment_map query_field t) src'
          |> Io.all
          |> Io.map ~f:List.Result.join
          |> Io.Result.map ~f:(fun field_values -> `List field_values)
        )
    | NonNullable t -> present ctx (Some src) fragment_map query_field t
    | Object o ->
        coerce_or_null src (fun src' ->
          let fields = collect_fields fragment_map o query_field.selection_set in
          resolve_fields ctx src' fragment_map o fields
        )
    | Enum e ->
        coerce_or_null src (fun src' ->
          match List.find (fun (v, s) -> src' == v) e.values with
          | Some (_, s) -> Io.ok (`String s)
          | None -> Io.ok `Null
        )

  and resolve_field : type src. 'ctx -> src -> fragment_map -> Graphql_parser.field -> ('ctx, src) field -> ((string * Yojson.Basic.json), string) result Io.t = fun ctx src fragment_map query_field (Field field) ->
    let open Io.Infix in
    let name = alias_or_name query_field in
    let resolver = field.resolve ctx src in
    match Arg.eval_arglist field.args query_field.arguments resolver with
    | Error _ as err -> Io.return err
    | Ok tmp ->
        field.lift tmp >>= fun resolved ->
        present ctx resolved fragment_map query_field field.typ >>|? fun value ->
        name, value

  and resolve_fields : type src. 'ctx -> src -> fragment_map -> ('ctx, src) obj -> Graphql_parser.field list -> (Yojson.Basic.json, string) result Io.t = fun ctx src fragment_map obj fields ->
    List.map (fun (query_field : Graphql_parser.field) ->
      match field_from_object obj query_field.name with
      | Some field ->
          resolve_field ctx src fragment_map query_field field
      | None ->
          Io.ok (alias_or_name query_field, `Null)
    ) fields
    |> Io.all
    |> Io.map ~f:List.Result.join
    |> Io.Result.map ~f:(fun properties -> `Assoc properties)

  let execute_operation : 'ctx schema -> fragment_map -> Graphql_parser.operation -> 'ctx -> (Yojson.Basic.json, string) result Io.t = fun schema fragment_map operation ctx ->
    match operation.optype with
    | Graphql_parser.Query ->
        let query  = schema.query in
        let fields = collect_fields fragment_map query operation.selection_set in
        resolve_fields ctx () fragment_map query fields
    | Graphql_parser.Mutation ->
        Io.error "Mutation is not implemented"
    | Graphql_parser.Subscription ->
        Io.error "Subscription is not implemented"

  let collect_fragments doc =
    List.fold_left (fun memo -> function
      | Graphql_parser.Operation _ -> memo
      | Graphql_parser.Fragment f -> StringMap.add f.name f memo
    ) StringMap.empty doc

  let rec select_operation = function
    | [] -> Error "No operation found"
    | (Graphql_parser.Operation op)::defs -> Ok op
    | _::defs -> select_operation defs

  let execute schema ctx doc =
    let open Io.Infix in
    let execute' schema ctx doc =
      let fragment_map = collect_fragments doc in
      let schema' = Introspection.add_schema_field schema in
      Io.return (select_operation doc) >>=? fun op ->
      execute_operation schema' fragment_map op ctx
    in
    execute' schema ctx doc >>| function
    | Ok data   -> Ok (`Assoc ["data", data])
    | Error err -> Error (`Assoc ["errors", `List [`Assoc ["message", `String err]]])
end