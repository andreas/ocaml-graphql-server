(* Helper modules *)
module List = struct
  include List
  let assoc_exn = assoc
  let assoc x ys = try Some (assoc_exn x ys) with Not_found -> None

  let find_exn = find
  let find cond xs = try Some (find_exn cond xs) with Not_found -> None

  module Result = struct
    let rec join ?(memo=[]) = function
      | [] -> Ok (List.rev memo)
      | (Error _ as err)::_ -> err
      | (Ok x)::xs -> join ~memo:(x::memo) xs

    let all f xs =
      List.map f xs |> join
  end
end

module Option = struct
  let return x = Some x
  let bind x f = match x with None -> None | Some y -> f y
  let map x ~f = match x with None -> None | Some y -> Some (f y)
  let or_default x default = match x with None -> default | Some x -> x
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

    let rec all ?(memo=[]) = function
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

    let rec map_s ?(memo=[]) f = function
      | [] -> Io.return (List.rev memo)
      | x::xs ->
          bind (f x) (fun x' -> map_s ~memo:(x'::memo) f xs)

    let rec map_p f xs =
      List.map f xs |> all

    module Infix = struct
      let (>>=) = bind
      let (>>|) = map
      let (>>=?) = Result.bind
      let (>>|?) = Result.map
    end
  end

  module StringMap = struct
    include Map.Make(String)
    exception Missing_key of string
    let find_exn key t = try find key t with Not_found -> raise (Missing_key key)
    let find k t = try Some (find_exn k t) with Missing_key _ -> None
  end

  module StringSet = Set.Make(String)

  type variable_map = Graphql_parser.const_value StringMap.t

  type deprecated =
    | NotDeprecated
    | Deprecated of string option

  type 'a enum_value = {
    name       : string;
    doc        : string option;
    deprecated : deprecated;
    value      : 'a;
  }

  let enum_value ?doc ?(deprecated=NotDeprecated) name ~value =
    { name; doc; deprecated; value; }

  let id : 'a. 'a -> 'a = fun x -> x

  module Arg = struct
    open Rresult

    type (_, _) arg_typ =
      | Scalar : {
          name   : string;
          doc    : string option;
          coerce : Graphql_parser.const_value -> ('b, string) result;
        } -> ('a, 'b option -> 'a) arg_typ
      | Object : {
          name   : string;
          doc    : string option;
          fields : ('a, 'b) arg_list;
          coerce : 'b;
        } -> ('c, 'a option -> 'c) arg_typ
      | Enum : {
          name   : string;
          doc    : string option;
          values : 'b enum_value list;
        } -> ('a, 'b option -> 'a) arg_typ
      | List : ('a, 'b -> 'a) arg_typ -> ('a, 'b list option -> 'a) arg_typ
      | NonNullable : ('a, 'b option -> 'a) arg_typ -> ('a, 'b -> 'a) arg_typ
    and ('a, 'b) arg =
      | Arg : {
          name : string;
          doc : string option;
          typ : ('a, 'b) arg_typ;
        } -> ('a, 'b) arg
      | DefaultArg : {
          name : string;
          doc : string option;
          typ : ('a, 'b option -> 'a) arg_typ;
          default : 'b;
        } -> ('a, 'b -> 'a) arg
    and (_, _) arg_list =
      | [] : ('a, 'a) arg_list
      | (::) : ('b, 'c -> 'b) arg * ('a, 'b) arg_list -> ('a, 'c -> 'b) arg_list

    let arg ?doc name ~typ =
      Arg { name; doc; typ }

    let arg' ?doc name ~typ ~default =
      DefaultArg { name; doc; typ; default }

    let scalar ?doc name ~coerce =
      Scalar { name; doc; coerce }

    let enum ?doc name ~values =
      Enum { name; doc; values }

    let obj ?doc name ~fields ~coerce =
      Object { name; doc; fields; coerce }

    (* Built-in argument types *)
    let int = Scalar {
      name = "Int";
      doc = None;
      coerce = function
        | `Int n -> Ok n
        | _ -> Error "Invalid int"
    }

    let string = Scalar {
      name = "String";
      doc = None;
      coerce = function
        | `String s -> Ok s
        | _ -> Error "Invalid string"
    }

    let float = Scalar {
      name = "Float";
      doc = None;
      coerce = function
        | `Float f -> Ok f
        | `Int n -> Ok (float_of_int n)
        | _ -> Error "Invalid float"
    }

    let bool = Scalar {
      name = "Boolean";
      doc = None;
      coerce = function
        | `Bool b -> Ok b
        | _ -> Error "Invalid boolean"
    }

    let guid = Scalar {
      name = "ID";
      doc = None;
      coerce = function
        | `String s -> Ok s
        | `Int n -> Ok (string_of_int n)
        | _ -> Error "Invalid ID"
    }

    let non_null typ = NonNullable typ
    let list typ = List typ

    let rec value_to_const_value variable_map = function
    | `Null -> `Null
    | `Int _ as i -> i
    | `Float _ as f -> f
    | `String _ as s -> s
    | `Bool _ as b -> b
    | `Enum _ as e -> e
    | `Variable v -> StringMap.find_exn v variable_map
    | `List xs -> `List (List.map (value_to_const_value variable_map) xs)
    | `Assoc props ->
        let props' = List.map (fun (name, value) -> name, value_to_const_value variable_map value) props in
        `Assoc props'

    let rec eval_arglist : type a b. variable_map -> (a, b) arg_list -> (string * Graphql_parser.value) list -> b -> (a, string) result =
      fun variable_map arglist key_values f ->
        match arglist with
        | [] -> Ok f
        | (DefaultArg arg)::arglist' ->
            let arglist'' = (Arg { name = arg.name; doc = arg.doc; typ = arg.typ })::arglist' in
            eval_arglist variable_map arglist'' key_values (function
              | None -> f arg.default
              | Some value -> f value
            )
        | (Arg arg)::arglist' ->
            try
              let value = List.assoc arg.name key_values in
              let const_value = Option.map value ~f:(value_to_const_value variable_map) in
              eval_arg variable_map arg.typ const_value >>= fun coerced ->
              eval_arglist variable_map arglist' key_values (f coerced)
            with StringMap.Missing_key key -> Error (Format.sprintf "Missing variable `%s`" key)

    and eval_arg : type a b. variable_map -> (a, b -> a) arg_typ -> Graphql_parser.const_value option -> (b, string) result = fun variable_map typ value ->
      match (typ, value) with
      | NonNullable _, None -> Error "Missing required argument"
      | NonNullable _, Some `Null -> Error "Missing required argument"
      | Scalar _, None -> Ok None
      | Scalar _, Some `Null -> Ok None
      | Object _, None -> Ok None
      | Object _, Some `Null -> Ok None
      | List _, None -> Ok None
      | List _, Some `Null -> Ok None
      | Enum _, None -> Ok None
      | Enum _, Some `Null -> Ok None
      | Scalar s, Some value ->
          s.coerce value >>| fun coerced ->
          Some coerced
      | Object o, Some value ->
          begin match value with
          | `Assoc props ->
              let props' = (props :> (string * Graphql_parser.value) list) in
              eval_arglist variable_map o.fields props' o.coerce >>| fun coerced ->
              Some coerced
          | _ -> Error "Expected object"
          end
     | List typ, Some value ->
          begin match value with
          | `List values ->
              let option_values = List.map (fun x -> Some x) values in
              List.Result.all (eval_arg variable_map typ) option_values >>| fun coerced ->
              Some coerced
          | value -> eval_arg variable_map typ (Some value) >>| fun coerced ->
              (Some [coerced] : b)
          end
      | NonNullable typ, value ->
          eval_arg variable_map typ value >>= (function
          | Some value -> Ok value
          | None -> Error "Missing required argument")
      | Enum e, Some value ->
          begin match value with
          | `Enum v
          | `String v ->
              begin match List.find (fun enum_value -> enum_value.name = v) e.values with
              | Some enum_value -> Ok (Some enum_value.value)
              | None -> Error "Invalid enum value"
              end
          | _ -> Error "Expected enum"
          end
  end

  (* Schema data types *)
  type 'a scalar = {
    name   : string;
    doc    : string option;
    coerce : 'a -> Yojson.Basic.json;
  }

  type 'a enum = {
    name    : string;
    doc     : string option;
    values  : 'a enum_value list;
  }

  type ('ctx, 'src) obj = {
    name   : string;
    doc    : string option;
    fields : ('ctx, 'src) field list Lazy.t;
  }
  and (_, _) field =
    Field : {
      name       : string;
      doc        : string option;
      deprecated : deprecated;
      typ        : ('ctx, 'io_out) typ;
      args       : ('maybe_io_out, 'args) Arg.arg_list;
      resolve    : 'ctx -> 'src -> 'args;
      lift       : 'maybe_io_out -> 'io_out Io.t
    } -> ('ctx, 'src) field
  and (_, _) typ =
    | Object      : ('ctx, 'src) obj -> ('ctx, 'src option) typ
    | List        : ('ctx, 'src) typ -> ('ctx, 'src list option) typ
    | NonNullable : ('ctx, 'src option) typ -> ('ctx, 'src) typ
    | Scalar      : 'src scalar -> ('ctx, 'src option) typ
    | Enum        : 'src enum -> ('ctx, 'src option) typ

  type 'ctx schema = {
    query : ('ctx, unit) obj;
    mutation : ('ctx, unit) obj option;
  }

  let schema ?(mutation_name="mutation") ?mutations ?(query_name="query") fields = {
    query = {
      name = query_name;
      doc = None;
      fields = lazy fields;
    };
    mutation = Option.map mutations ~f:(fun fields ->
      {
        name = mutation_name;
        doc = None;
        fields = lazy fields;
      }
    )
  }

  (* Constructor functions *)
  let obj ?doc name ~fields =
    let rec o = Object { name; doc; fields = lazy (fields o)} in
    o

  let field ?doc ?(deprecated=NotDeprecated) name ~typ ~args ~resolve =
    Field { lift = Io.return; name; doc; deprecated; typ; args; resolve }

  let io_field ?doc ?(deprecated=NotDeprecated) name ~typ ~args ~resolve =
    Field { lift = id; name; doc; deprecated; typ; args; resolve }

  let enum ?doc name ~values =
    Enum { name; doc; values }

  let scalar ?doc name ~coerce =
    Scalar { name; doc; coerce }

  let list typ =
    List typ

  let non_null typ =
    NonNullable typ

  (* Built-in scalars *)
  let int : 'ctx. ('ctx, int option) typ = Scalar {
    name   = "Int";
    doc    = None;
    coerce = fun i -> `Int i;
  }

  let string : 'ctx. ('ctx, string option) typ = Scalar {
    name   = "String";
    doc    = None;
    coerce = fun s ->`String s;
  }

  let bool : 'ctx. ('ctx, bool option) typ = Scalar {
    name   = "Boolean";
    doc    = None;
    coerce = fun b -> `Bool b;
  }

  let float : 'ctx. ('ctx, float option) typ = Scalar {
    name   = "Float";
    doc    = None;
    coerce = fun f -> `Float f;
  }

  let guid : 'ctx. ('ctx, string option) typ = Scalar {
    name   = "ID";
    doc    = None;
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
  type any_enum_value = AnyEnumValue : _ enum_value -> any_enum_value

  let unless_visited (result, visited) name f =
    if StringSet.mem name visited then
      result, visited
    else
      f (result, visited)

  (* Extracts all types contained in a single type *)
  let rec types : type src. ?memo:(any_typ list * StringSet.t) -> ('ctx, src) typ -> (any_typ list * StringSet.t) = fun ?(memo=([], StringSet.empty)) typ ->
    match typ with
    | List typ -> types ~memo typ
    | NonNullable typ -> types ~memo typ
    | Scalar s as scalar ->
        unless_visited memo s.name (fun (result, visited) ->
          (AnyTyp scalar)::result, StringSet.add s.name visited
        )
    | Enum e as enum ->
        unless_visited memo e.name (fun (result, visited) ->
          (AnyTyp enum)::result, StringSet.add e.name visited
        )
    | Object o as obj ->
        unless_visited memo o.name (fun (result, visited) ->
          let result'  = (AnyTyp obj)::result in
          let visited' = StringSet.add o.name visited in
          let reducer = fun memo (Field f) ->
            let result', visited' = types ~memo f.typ in
            arg_list_types result' f.args, visited'
          in
          List.fold_left reducer (result', visited') (Lazy.force o.fields)
        )
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
        let memo' = match arg with
        | Arg a -> arg_types memo a.typ
        | DefaultArg a -> arg_types memo a.typ
        in arg_list_types memo' args

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
    doc = None;
    values = [
      {
        name="SCALAR";
        doc=None;
        deprecated=NotDeprecated;
        value=`Scalar;
      };
      {
        name="OBJECT";
        doc=None;
        deprecated=NotDeprecated;
        value=`Object;
      };
      {
        name="INTERFACE";
        doc=None;
        deprecated=NotDeprecated;
        value=`Interface;
      };
      {
        name="UNION";
        doc=None;
        deprecated=NotDeprecated;
        value=`Union;
      };
      {
        name="ENUM";
        doc=None;
        deprecated=NotDeprecated;
        value=`Enum;
      };
      {
        name="INPUT_OBJECT";
        doc=None;
        deprecated=NotDeprecated;
        value=`InputObject;
      };
      {
        name="LIST";
        doc=None;
        deprecated=NotDeprecated;
        value=`List;
      };
      {
        name="NON_NULL";
        doc=None;
        deprecated=NotDeprecated;
        value=`NonNull;
      };
    ]
  }

  let __enum_value : 'ctx. ('ctx, any_enum_value option) typ = Object {
    name = "__EnumValue";
    doc = None;
    fields = lazy [
      Field {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ (AnyEnumValue enum_value) -> enum_value.name;
      };
      Field {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ (AnyEnumValue enum_value) -> enum_value.doc;
      };
      Field {
        name = "isDeprecated";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable bool;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ (AnyEnumValue enum_value) -> enum_value.deprecated <> NotDeprecated;
      };
      Field {
        name = "deprecationReason";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ (AnyEnumValue enum_value) ->
          match enum_value.deprecated with
          | Deprecated reason -> reason
          | NotDeprecated -> None
      }
    ]
  }

  let rec __input_value : 'ctx. ('ctx, any_arg option) typ = Object {
    name = "__InputValue";
    doc = None;
    fields = lazy [
      Field {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ (AnyArg arg) -> match arg with
          | Arg.DefaultArg a -> a.name
          | Arg.Arg a -> a.name
      };
      Field {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ (AnyArg arg) -> match arg with
          | Arg.DefaultArg a -> a.doc
          | Arg.Arg a -> a.doc
      };
      Field {
        name = "type";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ (AnyArg arg) -> match arg with
          | Arg.DefaultArg a -> AnyArgTyp a.typ
          | Arg.Arg a -> AnyArgTyp a.typ
      };
      Field {
        name = "defaultValue";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ (AnyArg v) -> None
      }
    ]
  }

  and __type : 'ctx . ('ctx, any_typ option) typ = Object {
    name = "__Type";
    doc = None;
    fields = lazy [
      Field {
        name = "kind";
        doc = None;
        deprecated = NotDeprecated;
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
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (Object o) -> Some o.name
          | AnyTyp (Scalar s) -> Some s.name
          | AnyTyp (Enum e) -> Some e.name
          | AnyArgTyp (Arg.Object o) -> Some o.name
          | AnyArgTyp (Arg.Scalar s) -> Some s.name
          | AnyArgTyp (Arg.Enum e) -> Some e.name
          | _ -> None;
      };
      Field {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (Object o) -> o.doc
          | AnyTyp (Scalar s) -> s.doc
          | AnyTyp (Enum e) -> e.doc
          | AnyArgTyp (Arg.Object o) -> o.doc
          | AnyArgTyp (Arg.Scalar s) -> s.doc
          | AnyArgTyp (Arg.Enum e) -> e.doc
          | _ -> None
      };
      Field {
        name = "fields";
        doc = None;
        deprecated = NotDeprecated;
        typ = List (NonNullable __field);
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (Object o) ->
              Some (List.map (fun f -> AnyField f) (Lazy.force o.fields))
          | AnyArgTyp (Arg.Object o) ->
              let arg_list = args_to_list o.fields in
              Some (List.map (fun (AnyArg f) -> AnyArgField f) arg_list)
          | _ -> None
      };
      Field {
        name = "interfaces";
        doc = None;
        deprecated = NotDeprecated;
        typ = List __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (Object _) -> Some []
          | _ -> None
      };
      Field {
        name = "possibleTypes";
        doc = None;
        deprecated = NotDeprecated;
        typ = List __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> None
      };
      Field {
        name = "ofType";
        doc = None;
        deprecated = NotDeprecated;
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
        doc = None;
        deprecated = NotDeprecated;
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
        doc = None;
        deprecated = NotDeprecated;
        typ = List (NonNullable __enum_value);
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ t -> match t with
          | AnyTyp (Enum e) -> Some (List.map (fun x -> AnyEnumValue x) e.values)
          | AnyArgTyp (Arg.Enum e) -> Some (List.map (fun x -> AnyEnumValue x) e.values)
          | _      -> None
      }
    ]
  }

  and __field : 'ctx. ('ctx, any_field option) typ = Object {
    name = "__Field";
    doc = None;
    fields = lazy [
      Field {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> match f with
          | AnyField (Field f) -> f.name
          | AnyArgField (Arg.Arg a) -> a.name
          | AnyArgField (Arg.DefaultArg a) -> a.name
      };
      Field {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> match f with
          | AnyField (Field f) -> f.doc
          | AnyArgField (Arg.Arg a) -> a.doc
          | AnyArgField (Arg.DefaultArg a) -> a.doc
      };
      Field {
        name = "args";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable __input_value));
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> match f with
          | AnyField (Field f) -> args_to_list f.args
          | AnyArgField _ -> []
      };
      Field {
        name = "type";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> match f with
          | AnyField (Field f) -> AnyTyp f.typ
          | AnyArgField (Arg.Arg a) -> AnyArgTyp a.typ
          | AnyArgField (Arg.DefaultArg a) -> AnyArgTyp a.typ
      };
      Field {
        name = "isDeprecated";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable bool;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> match f with
          | AnyField (Field { deprecated = Deprecated _ }) -> true
          | _ -> false
      };
      Field {
        name = "deprecationReason";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ f -> match f with
          | AnyField (Field { deprecated = Deprecated reason }) -> reason
          | _ -> None
      }
    ]
  }

  let __directive = Object {
    name = "__Directive";
    doc = None;
    fields = lazy [
      Field {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ d -> d.name
      }
    ]
  }

  let __schema : 'ctx. ('ctx, 'ctx schema option) typ = Object {
    name = "__Schema";
    doc = None;
    fields = lazy [
      Field {
        name = "types";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable __type));
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ s ->
          let query_types, visited = types (Object s.query) in
          match s.mutation with
          | None -> query_types
          | Some mut -> fst @@ types ~memo:(query_types, visited) (Object mut)
      };
      Field {
        name = "queryType";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ s -> AnyTyp (Object s.query)
      };
      Field {
        name = "mutationType";
        doc = None;
        deprecated = NotDeprecated;
        typ = __type;
        args = Arg.[];
        lift = Io.return;
        resolve = fun _ s -> Option.map s.mutation ~f:(fun mut -> AnyTyp (Object mut))
      };
      Field {
        name = "directives";
        doc = None;
        deprecated = NotDeprecated;
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
      doc = None;
      deprecated = NotDeprecated;
      typ = NonNullable __schema;
      args = Arg.[];
      lift = Io.return;
      resolve = fun _ _ -> s
    } in
    let fields = lazy (schema_field::(Lazy.force s.query.fields)) in
    { s with query = { s.query with fields } }
end

  (* Execution *)
  type variables = (string * Graphql_parser.const_value) list
  type json_variables = (string * Yojson.Basic.json) list
  type fragment_map = Graphql_parser.fragment StringMap.t
  type execution_order = Serial | Parallel
  type 'ctx execution_context = {
    variables : variable_map;
    fragments : fragment_map;
    ctx       : 'ctx;
  }

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
    List.find (fun (Field field) -> field.name = field_name) (Lazy.force obj.fields)

  let coerce_or_null : 'a option -> ('a -> (Yojson.Basic.json, string) result Io.t) -> (Yojson.Basic.json, string) result Io.t = fun src f ->
    match src with
    | None -> Io.ok `Null
    | Some src' -> f src'

  let map order =
    match order with
    | Serial -> Io.map_s ~memo:[]
    | Parallel -> Io.map_p

  let rec present : type src. 'ctx execution_context -> src -> Graphql_parser.field -> ('ctx, src) typ -> (Yojson.Basic.json, string) result Io.t = fun ctx src query_field typ ->
    match typ with
    | Scalar s -> coerce_or_null src (fun x -> Io.return (Ok (s.coerce x)))
    | List t ->
        coerce_or_null src (fun src' ->
          List.map (fun x -> present ctx x query_field t) src'
          |> Io.all
          |> Io.map ~f:List.Result.join
          |> Io.Result.map ~f:(fun field_values -> `List field_values)
        )
    | NonNullable t -> present ctx (Some src) query_field t
    | Object o ->
        coerce_or_null src (fun src' ->
          let fields = collect_fields ctx.fragments o query_field.selection_set in
          resolve_fields ctx src' o fields
        )
    | Enum e ->
        coerce_or_null src (fun src' ->
          match List.find (fun enum_value -> src' == enum_value.value) e.values with
          | Some enum_value -> Io.ok (`String enum_value.name)
          | None -> Io.ok `Null
        )

  and resolve_field : type src. 'ctx execution_context -> src -> Graphql_parser.field -> ('ctx, src) field -> ((string * Yojson.Basic.json), string) result Io.t = fun ctx src query_field (Field field) ->
    let open Io.Infix in
    let name = alias_or_name query_field in
    let resolver = field.resolve ctx.ctx src in
    match Arg.eval_arglist ctx.variables field.args query_field.arguments resolver with
    | Error _ as err -> Io.return err
    | Ok tmp ->
        field.lift tmp >>= fun resolved ->
        present ctx resolved query_field field.typ >>|? fun value ->
        name, value

  and resolve_fields : type src. 'ctx execution_context -> ?execution_order:execution_order -> src -> ('ctx, src) obj -> Graphql_parser.field list -> (Yojson.Basic.json, string) result Io.t = fun ctx ?execution_order:(execution_order=Parallel) src obj fields ->
    map execution_order (fun (query_field : Graphql_parser.field) ->
      if query_field.name = "__typename" then
        Io.ok (alias_or_name query_field, `String obj.name)
      else
        match field_from_object obj query_field.name with
        | Some field ->
            resolve_field ctx src query_field field
        | None ->
            Io.ok (alias_or_name query_field, `Null)
    ) fields
    |> Io.map ~f:List.Result.join
    |> Io.Result.map ~f:(fun properties -> `Assoc properties)

  let execute_operation : 'ctx schema -> 'ctx execution_context -> fragment_map -> variable_map -> Graphql_parser.operation -> (Yojson.Basic.json, string) result Io.t = fun schema ctx fragments variables operation ->
    match operation.optype with
    | Graphql_parser.Query ->
        let query  = schema.query in
        let fields = collect_fields fragments query operation.selection_set in
        resolve_fields ctx () query fields
    | Graphql_parser.Mutation ->
        begin match schema.mutation with
        | None -> Io.error "Schema is not configured for mutations"
        | Some mut ->
            let fields = collect_fields fragments mut operation.selection_set in
            resolve_fields ~execution_order:Serial ctx () mut fields
        end
    | Graphql_parser.Subscription ->
        Io.error "Subscription is not implemented"

  let collect_fragments doc =
    List.fold_left (fun memo -> function
      | Graphql_parser.Operation _ -> memo
      | Graphql_parser.Fragment f -> StringMap.add f.name f memo
    ) StringMap.empty doc

  let collect_operations doc =
    List.fold_left (fun memo -> function
      | Graphql_parser.Operation op -> op::memo
      | Graphql_parser.Fragment _ -> memo
    ) [] doc

  let rec select_operation ?operation_name doc =
    let operations = collect_operations doc in
    match operation_name, operations with
    | _, [] -> Error "No operation found"
    | None, [op] -> Ok op
    | None, _::_ -> Error "Operation name required"
    | Some name, ops ->
        try
          Ok (List.find_exn (fun op -> op.Graphql_parser.name = Some name) ops)
        with Not_found ->
          Error "Operation not found"

  let execute schema ctx ?variables:(variables=[]) ?operation_name doc =
    let open Io.Infix in
    let execute' schema ctx doc =
      let fragments = collect_fragments doc in
      let variables = List.fold_left (fun memo (name, value) -> StringMap.add name value memo) StringMap.empty variables in
      let execution_ctx = { fragments; ctx; variables } in
      let schema' = Introspection.add_schema_field schema in
      Io.return (select_operation ?operation_name doc) >>=? fun op ->
      execute_operation schema' execution_ctx fragments variables op
    in
    execute' schema ctx doc >>| function
    | Ok data   -> Ok (`Assoc ["data", data])
    | Error err -> Error (`Assoc ["errors", `List [`Assoc ["message", `String err]]])
end
