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
  let map x ~f = match x with None -> None | Some y -> Some (f y)
end

(* IO *)
module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  module Stream : sig
    type +'a io
    type 'a t

    val map : 'a t -> ('a -> 'b io) -> 'b t
    val iter : 'a t -> ('a -> unit io) -> unit io
    val close : 'a t -> unit
  end with type 'a io := 'a t
end

(* Field_error *)
module type Field_error = sig
  type t
  val message_of_field_error : t -> string
  val extensions_of_field_error : t -> (string * Yojson.Basic.json [@warning "-3"]) list option
end

(* Schema *)
module Make (Io : IO) (Field_error: Field_error) = struct
  module Io = struct
    include Io

    let map x ~f = bind x (fun x' -> return (f x'))
    let ok x = Io.return (Ok x)
    let error x = Io.return (Error x)

    let rec all = function
      | [] -> Io.return []
      | x::xs ->
          bind (all xs) (fun xs' ->
            map x ~f:(fun x' -> x'::xs')
          )

    module Result = struct
      let bind x f = bind x (function Ok x' -> f x' | Error _ as err -> Io.return err)
      let map_error x ~f = map x ~f:(function Ok _ as ok -> ok | Error err -> Error (f err))
      let map x ~f = map x ~f:(function Ok x' -> Ok (f x') | Error _ as err -> err)
    end

    let rec map_s ?(memo=[]) f = function
      | [] -> Io.return (List.rev memo)
      | x::xs ->
          bind (f x) (fun x' -> map_s ~memo:(x'::memo) f xs)

    let map_p f xs = List.map f xs |> all

    module Infix = struct
      let (>>|) x f = map x ~f
      let (>>=?) = Result.bind
    end
  end

  module StringMap = struct
    include Map.Make(String)
    exception Missing_key of string
    let find_exn key t = try find key t with Not_found -> raise (Missing_key key)
    let find k t = try Some (find_exn k t) with Missing_key _ -> None
  end

  module StringSet = Set.Make(String)

  type field_error = Field_error.t

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

  type json = Yojson.Basic.json [@warning "-3"]

  let enum_value ?doc ?(deprecated=NotDeprecated) name ~value =
    { name; doc; deprecated; value; }

  let id : 'a. 'a -> 'a = fun x -> x

  module Arg = struct
    open Rresult

    type _ arg_typ =
      | Scalar : {
          name   : string;
          doc    : string option;
          coerce : Graphql_parser.const_value -> ('a, string) result;
        } -> 'a option arg_typ
      | Object : {
          name   : string;
          doc    : string option;
          fields : ('a, 'b) arg_list;
          coerce : 'b;
        } -> 'a option arg_typ
      | Enum : {
          name   : string;
          doc    : string option;
          values : 'a enum_value list;
        } -> 'a option arg_typ
      | List : 'a arg_typ -> 'a list option arg_typ
      | NonNullable : 'a option arg_typ -> 'a arg_typ
    and _ arg =
      | Arg : {
          name : string;
          doc : string option;
          typ : 'a arg_typ;
        } -> 'a arg
      | DefaultArg : {
          name : string;
          doc : string option;
          typ : 'a option arg_typ;
          default : 'a;
        } -> 'a arg
    and (_, _) arg_list =
      | [] : ('a, 'a) arg_list
      | (::) : 'a arg * ('b, 'c) arg_list -> ('b, 'a -> 'c) arg_list

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

    let rec string_of_const_value : Graphql_parser.const_value -> string = function
      | `Null -> "null"
      | `Int i -> string_of_int i
      | `Float f -> string_of_float f
      | `String s -> Printf.sprintf "\"%s\"" s
      | `Bool b -> string_of_bool b
      | `Enum e -> e
      | `List l ->
          let values = List.map (fun i -> string_of_const_value i) l in
          Printf.sprintf "[%s]" (String.concat ", " values)
      | `Assoc a ->
          let values =
            List.map
              (fun (k, v) ->
                Printf.sprintf "%s: %s" k (string_of_const_value v) )
              a
          in
          Printf.sprintf "{%s}" (String.concat ", " values)

    let rec string_of_arg_typ : type a. a arg_typ -> string = function
      | Scalar a -> a.name
      | Object a -> a.name
      | Enum a -> a.name
      | List a -> Printf.sprintf "[%s]" (string_of_arg_typ a)
      | NonNullable a -> Printf.sprintf "%s!" (string_of_arg_typ a)

    let eval_arg_error ?(field_type="field") ~field_name ~arg_name arg_typ value =
      let found_str =
        match value with
        | Some v -> Printf.sprintf "found %s" (string_of_const_value v)
        | None -> "but not provided"
      in
      Printf.sprintf "Argument `%s` of type `%s` expected on %s `%s`, %s."
        arg_name
        (string_of_arg_typ arg_typ)
        field_type
        field_name
        found_str

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

    let rec eval_arglist
      : type a b. variable_map
      -> ?field_type:string
      -> field_name:string
      -> (a, b) arg_list
      -> (string * Graphql_parser.value) list
      -> b
      -> (a, string) result =
      fun variable_map ?field_type ~field_name arglist key_values f ->
        match arglist with
        | [] -> Ok f
        | (DefaultArg arg)::arglist' ->
            let arglist'' = (Arg { name = arg.name; doc = arg.doc; typ = arg.typ })::arglist' in
            eval_arglist variable_map ?field_type ~field_name arglist'' key_values (function
              | None -> f arg.default
              | Some value -> f value
            )
        | (Arg arg)::arglist' ->
            try
              let value = List.assoc arg.name key_values in
              let const_value = Option.map value ~f:(value_to_const_value variable_map) in
              eval_arg variable_map ?field_type ~field_name ~arg_name:arg.name arg.typ const_value >>= fun coerced ->
              eval_arglist variable_map ?field_type ~field_name arglist' key_values (f coerced)
            with StringMap.Missing_key key -> Error (Format.sprintf "Missing variable `%s`" key)

    and eval_arg
      : type a. variable_map
      -> ?field_type:string
      -> field_name:string
      -> arg_name:string
      -> a arg_typ
      -> Graphql_parser.const_value option
      -> (a, string) result =
      fun variable_map ?field_type ~field_name ~arg_name typ value ->
        match (typ, value) with
        | NonNullable _, None -> Error (eval_arg_error ?field_type ~field_name ~arg_name typ value)
        | NonNullable _, Some `Null -> Error (eval_arg_error ?field_type ~field_name ~arg_name typ value)
        | Scalar _, None -> Ok None
        | Scalar _, Some `Null -> Ok None
        | Object _, None -> Ok None
        | Object _, Some `Null -> Ok None
        | List _, None -> Ok None
        | List _, Some `Null -> Ok None
        | Enum _, None -> Ok None
        | Enum _, Some `Null -> Ok None
        | Scalar s, Some value ->
          begin match (s.coerce value) with
          | Ok coerced -> Ok (Some coerced)
          | Error _ -> Error (eval_arg_error ?field_type ~field_name ~arg_name typ (Some value))
          end
        | Object o, Some value ->
            begin match value with
            | `Assoc props ->
                let props' = (props :> (string * Graphql_parser.value) list) in
                eval_arglist variable_map ?field_type ~field_name o.fields props' o.coerce >>| fun coerced ->
                Some coerced
            | _ -> Error (eval_arg_error ?field_type ~field_name ~arg_name typ (Some value))
            end
      | List typ, Some value ->
            begin match value with
            | `List values ->
                let option_values = List.map (fun x -> Some x) values in
                List.Result.all (eval_arg variable_map ?field_type ~field_name ~arg_name typ) option_values >>| fun coerced ->
                Some coerced
            | value -> eval_arg variable_map ?field_type ~field_name ~arg_name typ (Some value) >>| fun coerced ->
                (Some [coerced] : a)
            end
        | NonNullable typ, value ->
            eval_arg variable_map ?field_type ~field_name ~arg_name typ value >>= (function
            | Some value -> Ok value
            | None -> Error (eval_arg_error ?field_type ~field_name ~arg_name typ None))
        | Enum e, Some value ->
            begin match value with
            | `Enum v
            | `String v ->
                begin match List.find (fun enum_value -> enum_value.name = v) e.values with
                | Some enum_value -> Ok (Some enum_value.value)
                | None -> Error (Printf.sprintf "Invalid enum value for argument `%s` on field `%s`" arg_name field_name)
                end
            | _ -> Error (Printf.sprintf "Expected enum for argument `%s` on field `%s`" arg_name field_name)
            end
  end

  (* Schema data types *)
  type 'a scalar = {
    name   : string;
    doc    : string option;
    coerce : 'a -> json;
  }

  type 'a enum = {
    name    : string;
    doc     : string option;
    values  : 'a enum_value list;
  }

  type fragment_map = Graphql_parser.fragment StringMap.t
  type 'ctx resolve_info = {
    ctx : 'ctx;
    field : Graphql_parser.field;
    fragments : fragment_map;
    variables : variable_map;
  }

  type ('ctx, 'src) obj = {
    name   : string;
    doc    : string option;
    fields : ('ctx, 'src) field list Lazy.t;
    abstracts : abstract list ref;
  }
  and (_, _) field =
    Field : {
      name       : string;
      doc        : string option;
      deprecated : deprecated;
      typ        : ('ctx, 'out) typ;
      args       : ('a, 'args) Arg.arg_list;
      resolve    : 'ctx resolve_info -> 'src -> 'args;
      lift       : 'a -> ('out, field_error) result Io.t;
    } -> ('ctx, 'src) field
  and (_, _) typ =
    | Object      : ('ctx, 'src) obj -> ('ctx, 'src option) typ
    | List        : ('ctx, 'src) typ -> ('ctx, 'src list option) typ
    | NonNullable : ('ctx, 'src option) typ -> ('ctx, 'src) typ
    | Scalar      : 'src scalar -> ('ctx, 'src option) typ
    | Enum        : 'src enum -> ('ctx, 'src option) typ
    | Abstract    : abstract -> ('ctx, ('ctx, 'a) abstract_value option) typ
  and any_typ =
    | AnyTyp : (_, _) typ -> any_typ
    | AnyArgTyp : _ Arg.arg_typ -> any_typ
  and abstract = {
    name   : string;
    doc    : string option;
    kind   : [`Union | `Interface of abstract_field list Lazy.t];
    mutable types  : any_typ list;
  }
  and abstract_field =
    AbstractField : (_, _) field -> abstract_field
  and ('ctx, 'a) abstract_value =
    AbstractValue : ('ctx, 'src option) typ * 'src -> ('ctx, 'a) abstract_value

  type 'ctx subscription_field =
    SubscriptionField : {
      name       : string;
      doc        : string option;
      deprecated : deprecated;
      typ        : ('ctx, 'out) typ;
      args       : (('out Io.Stream.t, field_error) result Io.t, 'args) Arg.arg_list;
      resolve    : 'ctx resolve_info -> 'args;
    } -> 'ctx subscription_field

  type 'ctx subscription_obj = {
    name   : string;
    doc    : string option;
    fields : 'ctx subscription_field list;
  }

  type ('ctx, 'a) abstract_typ = ('ctx, ('ctx, 'a) abstract_value option) typ

  type directive_location = [
    | `Query
    | `Mutation
    | `Subscription
    | `Field
    | `Fragment_definition
    | `Fragment_spread
    | `Inline_fragment
    | `Variable_definition
    ]

  type directive =
    Directive : {
      name       : string;
      doc        : string option;
      locations  : directive_location list;
      args       : ([ `Skip | `Include ], 'args) Arg.arg_list;
      resolve    : 'args;
    } -> directive

  type 'ctx schema = {
    query : ('ctx, unit) obj;
    mutation : ('ctx, unit) obj option;
    subscription : 'ctx subscription_obj option;
  }

  let schema ?(mutation_name="mutation")
             ?mutations
             ?(subscription_name="subscription")
             ?subscriptions
             ?(query_name="query")
             fields = {
    query = {
      name = query_name;
      doc = None;
      abstracts = ref [];
      fields = lazy fields;
    };
    mutation = Option.map mutations ~f:(fun fields ->
      {
        name = mutation_name;
        doc = None;
        abstracts = ref [];
        fields = lazy fields;
      }
    );
    subscription = Option.map subscriptions ~f:(fun fields ->
      {
        name = subscription_name;
        doc = None;
        fields;
      }
    )
  }

  (* Constructor functions *)
  let obj ?doc name ~fields =
    let rec o = Object { name; doc; fields = lazy (fields o); abstracts = ref []} in
    o

  let field ?doc ?(deprecated=NotDeprecated) name ~typ ~args ~resolve =
    Field { name; doc; deprecated; typ; args; resolve; lift = Io.ok }

  let io_field ?doc ?(deprecated=NotDeprecated) name ~typ ~args ~resolve =
    Field { name; doc; deprecated; typ; args; resolve; lift = id }

  let abstract_field ?doc ?(deprecated=NotDeprecated) name ~typ ~args =
    AbstractField (Field { lift = Io.ok; name; doc; deprecated; typ; args; resolve = Obj.magic () })

  let subscription_field ?doc ?(deprecated=NotDeprecated) name ~typ ~args ~resolve =
    SubscriptionField { name; doc; deprecated; typ; args; resolve }

  let enum ?doc name ~values =
    Enum { name; doc; values }

  let scalar ?doc name ~coerce =
    Scalar { name; doc; coerce }

  let list typ =
    List typ

  let non_null typ =
    NonNullable typ

  let union ?doc name =
    Abstract { name; doc; types = []; kind = `Union }

  let interface ?doc name ~fields =
    let rec i = Abstract { name; doc; types = []; kind = `Interface (lazy (fields i)) } in
    i

  let add_type abstract_typ typ =
    match (abstract_typ, typ) with
    | Abstract a, Object o ->
        (* TODO add subtype check here *)
        a.types <- (AnyTyp typ)::a.types;
        o.abstracts := a :: !(o.abstracts);
        fun src -> AbstractValue (typ, src)
    | _ ->
        invalid_arg "Arguments must be Interface/Union and Object"

  let obj_of_subscription_obj {name; doc; fields} =
    let fields = List.map
      (fun (SubscriptionField {name; doc; deprecated; typ; args; resolve}) ->
        Field { lift = Obj.magic (); name; doc; deprecated; typ; args; resolve = (fun ctx () -> resolve ctx) })
      fields
    in
    { name; doc; abstracts = ref []; fields = lazy fields }

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

  (* Mandatory directives: skip and include *)
  let skip_directive = Directive {
    name = "skip";
    doc = Some "Directs the executor to skip this field or fragment when the `if` argument is true.";
    locations = [`Field; `Fragment_spread; `Inline_fragment];
    args = Arg.[
      arg "if" ~doc:"Skipped when true." ~typ:(non_null bool)
    ];
    resolve = function
      | true -> `Skip
      | false -> `Include
  }

  let include_directive = Directive {
    name = "include";
    doc = Some "Directs the executor to include this field or fragment only when the `if` argument is true.";
    locations = [`Field; `Fragment_spread; `Inline_fragment];
    args = Arg.[
      arg "if" ~doc:"Included when true." ~typ:(non_null bool)
    ];
    resolve = function
      | true -> `Include
      | false -> `Skip
  }

module Introspection = struct
  (* any_typ, any_field and any_arg hide type parameters to avoid scope escaping errors *)
  type any_field =
    | AnyField : (_, _) field -> any_field
    | AnyArgField : _ Arg.arg -> any_field
  type any_arg = AnyArg : _ Arg.arg -> any_arg
  type any_enum_value = AnyEnumValue : _ enum_value -> any_enum_value

  let unless_visited (result, visited) name f =
    if StringSet.mem name visited then
      result, visited
    else
      f (result, visited)

  (* Extracts all types contained in a single type *)
  let rec types : type ctx src. ?memo:(any_typ list * StringSet.t) -> (ctx, src) typ -> (any_typ list * StringSet.t) = fun ?(memo=([], StringSet.empty)) typ ->
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
            let memo' = types ~memo f.typ in
            arg_list_types memo' f.args
          in
          List.fold_left reducer (result', visited') (Lazy.force o.fields)
        )
   | Abstract a as abstract ->
      unless_visited memo a.name (fun (result, visited) ->
        let result' = (AnyTyp abstract)::result in
        let visited' = StringSet.add a.name visited in
        List.fold_left (fun memo typ -> match typ with
          | AnyTyp typ -> types ~memo typ
          | AnyArgTyp _ -> failwith "Abstracts can't have argument types")
          (result', visited') a.types
      )

  and arg_types : type a. (any_typ list * StringSet.t) -> a Arg.arg_typ -> (any_typ list * StringSet.t) = fun memo argtyp ->
    match argtyp with
    | Arg.List typ -> arg_types memo typ
    | Arg.NonNullable typ -> arg_types memo typ
    | Arg.Scalar s as scalar ->
        unless_visited memo s.name (fun (result, visited) ->
          (AnyArgTyp scalar)::result, StringSet.add s.name visited
        )
    | Arg.Enum e as enum ->
        unless_visited memo e.name (fun (result, visited) ->
          (AnyArgTyp enum)::result, StringSet.add e.name visited
        )
    | Arg.Object o as obj ->
        unless_visited memo o.name (fun (result, visited) ->
          let memo' = (AnyArgTyp obj)::result, StringSet.add o.name visited in
          arg_list_types memo' o.fields
        )
  and arg_list_types : type a b. (any_typ list * StringSet.t) -> (a, b) Arg.arg_list -> (any_typ list * StringSet.t) = fun memo arglist ->
    let open Arg in
    match arglist with
    | [] -> memo
    | arg::args ->
        let memo' = match arg with
        | Arg a -> arg_types memo a.typ
        | DefaultArg a -> arg_types memo a.typ
        in arg_list_types memo' args

  let types_of_schema s =
    let types, _ =
      List.fold_left
      (fun memo op ->
        match op with
        | None -> memo
        | Some op -> types ~memo (Object op))
      ([], StringSet.empty)
      [Some s.query; s.mutation; Option.map s.subscription ~f:obj_of_subscription_obj]
    in
    types

  let rec args_to_list : type a b. ?memo:any_arg list -> (a, b) Arg.arg_list -> any_arg list = fun ?memo:(memo=[]) arglist ->
    let open Arg in
    match arglist with
    | [] ->
        memo
    | arg::args ->
        let memo' = List.cons (AnyArg arg) memo in
        args_to_list ~memo:memo' args

  let no_abstracts = ref []

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
    abstracts = no_abstracts;
    fields = lazy [
      Field {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (AnyEnumValue enum_value) -> enum_value.name;
      };
      Field {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (AnyEnumValue enum_value) -> enum_value.doc;
      };
      Field {
        name = "isDeprecated";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable bool;
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (AnyEnumValue enum_value) -> enum_value.deprecated <> NotDeprecated;
      };
      Field {
        name = "deprecationReason";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.ok;
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
    abstracts = no_abstracts;
    fields = lazy [
      Field {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = Io.ok;
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
        lift = Io.ok;
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
        lift = Io.ok;
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
        lift = Io.ok;
        resolve = fun _ (AnyArg _) -> None
      }
    ]
  }

  and __type : 'ctx . ('ctx, any_typ option) typ = Object {
    name = "__Type";
    doc = None;
    abstracts = no_abstracts;
    fields = lazy [
      Field {
        name = "kind";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable __type_kind;
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ t -> match t with
          | AnyTyp (Object _) -> `Object
          | AnyTyp (Abstract { kind = `Union; _ }) -> `Union
          | AnyTyp (Abstract { kind = `Interface _; _ }) -> `Interface
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
        lift = Io.ok;
        resolve = fun _ t -> match t with
          | AnyTyp (Object o) -> Some o.name
          | AnyTyp (Scalar s) -> Some s.name
          | AnyTyp (Enum e) -> Some e.name
          | AnyTyp (Abstract a) -> Some a.name
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
        lift = Io.ok;
        resolve = fun _ t -> match t with
          | AnyTyp (Object o) -> o.doc
          | AnyTyp (Scalar s) -> s.doc
          | AnyTyp (Enum e) -> e.doc
          | AnyTyp (Abstract a) -> a.doc
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
        lift = Io.ok;
        resolve = fun _ t -> match t with
          | AnyTyp (Object o) ->
              Some (List.map (fun f -> AnyField f) (Lazy.force o.fields))
          | AnyTyp (Abstract { kind = `Interface fields; _ }) ->
              Some (List.map (fun (AbstractField f) -> AnyField f) (Lazy.force fields))
          | AnyArgTyp (Arg.Object o) ->
              let arg_list = args_to_list o.fields in
              Some (List.map (fun (AnyArg f) -> AnyArgField f) arg_list)
          | _ -> None
      };
      Field {
        name = "interfaces";
        doc = None;
        deprecated = NotDeprecated;
        typ = List (NonNullable __type);
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ t -> match t with
          | AnyTyp (Object o) ->
              let interfaces = List.filter (function | { kind = `Interface _; _} -> true | _ -> false) !(o.abstracts) in
              Some (List.map (fun i -> AnyTyp (Abstract i)) interfaces)
          | _ -> None
      };
      Field {
        name = "possibleTypes";
        doc = None;
        deprecated = NotDeprecated;
        typ = List (NonNullable __type);
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ t -> match t with
          | AnyTyp (Abstract a) ->
              Some a.types
          | _ -> None
      };
      Field {
        name = "ofType";
        doc = None;
        deprecated = NotDeprecated;
        typ = __type;
        args = Arg.[];
        lift = Io.ok;
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
        lift = Io.ok;
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
        lift = Io.ok;
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
    abstracts = no_abstracts;
    fields = lazy [
      Field {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = Io.ok;
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
        lift = Io.ok;
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
        lift = Io.ok;
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
        lift = Io.ok;
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
        lift = Io.ok;
        resolve = fun _ f -> match f with
          | AnyField (Field { deprecated = Deprecated _; _ }) -> true
          | _ -> false
      };
      Field {
        name = "deprecationReason";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ f -> match f with
          | AnyField (Field { deprecated = Deprecated reason; _ }) -> reason
          | _ -> None
      }
    ]
  }

  let __directive_location = Enum {
    name = "__DirectiveLocation";
    doc = None;
    values = [
      {
        name="QUERY";
        doc=None;
        deprecated=NotDeprecated;
        value=`Query;
      };
      {
        name="MUTATION";
        doc=None;
        deprecated=NotDeprecated;
        value=`Mutation;
      };
      {
        name="SUBSCRIPTION";
        doc=None;
        deprecated=NotDeprecated;
        value=`Subscription;
      };
      {
        name="FIELD";
        doc=None;
        deprecated=NotDeprecated;
        value=`Field;
      };
      {
        name="FRAGMENT_DEFINITION";
        doc=None;
        deprecated=NotDeprecated;
        value=`Fragment_definition;
      };
      {
        name="FRAGMENT_SPREAD";
        doc=None;
        deprecated=NotDeprecated;
        value=`Fragment_spread;
      };
      {
        name="INLINE_FRAGMENT";
        doc=None;
        deprecated=NotDeprecated;
        value=`Inline_fragment;
      };
      {
        name="VARIABLE_DEFINITION";
        doc=None;
        deprecated=NotDeprecated;
        value=`Variable_definition;
      };
    ]
  }

  let __directive = Object {
    name = "__Directive";
    doc = None;
    abstracts = no_abstracts;
    fields = lazy [
      Field {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (Directive d) -> d.name
      };
      Field {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (Directive d) -> d.doc
      };
      Field {
        name = "locations";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable __directive_location));
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (Directive d) -> d.locations
      };
      Field {
        name = "args";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable __input_value));
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (Directive d) -> args_to_list d.args
      }
    ]
  }

  let __schema : 'ctx. ('ctx, ('ctx schema * any_typ list) option) typ = Object {
    name = "__Schema";
    doc = None;
    abstracts = no_abstracts;
    fields = lazy [
      Field {
        name = "types";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable __type));
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (_schema, types) -> types
      };
      Field {
        name = "queryType";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable __type;
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (schema, _types) -> AnyTyp (Object schema.query)
      };
      Field {
        name = "mutationType";
        doc = None;
        deprecated = NotDeprecated;
        typ = __type;
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (schema, _types) -> Option.map schema.mutation ~f:(fun mut -> AnyTyp (Object mut))
      };
      Field {
        name = "subscriptionType";
        doc = None;
        deprecated = NotDeprecated;
        typ = __type;
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ (schema, _types) ->
          Option.map schema.subscription ~f:(fun subs -> AnyTyp (Object (obj_of_subscription_obj subs)))
      };
      Field {
        name = "directives";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable __directive));
        args = Arg.[];
        lift = Io.ok;
        resolve = fun _ _ -> []
      }
    ]
  }

  let add_built_in_fields schema =
    let types = types_of_schema schema in
    let schema_field = Field {
      name = "__schema";
      doc = None;
      deprecated = NotDeprecated;
      typ = NonNullable __schema;
      args = Arg.[];
      lift = Io.ok;
      resolve = fun _ _ -> (schema, types)
    } in
    let type_field = Field {
      name = "__type";
      doc = None;
      deprecated = NotDeprecated;
      typ = __type;
      args = Arg.[arg "name" ~typ:(non_null string)];
      lift = Io.ok;
      resolve = fun _ _ name ->
        List.find (fun typ ->
          match typ with
          | AnyTyp (Object o) -> o.name = name
          | AnyTyp (Scalar s) -> s.name = name
          | AnyTyp (Enum e) -> e.name = name
          | AnyTyp (Abstract a) -> a.name = name
          | AnyTyp (List _) -> false
          | AnyTyp (NonNullable _) -> false
          | AnyArgTyp (Arg.Object o) -> o.name = name
          | AnyArgTyp (Arg.Scalar s) -> s.name = name
          | AnyArgTyp (Arg.Enum e) -> e.name = name
          | AnyArgTyp (Arg.List _) -> false
          | AnyArgTyp (Arg.NonNullable _) -> false
        ) types
    } in
    let fields = lazy (schema_field::type_field::(Lazy.force schema.query.fields)) in
    { schema with query = { schema.query with fields } }
end

  (* Execution *)
  type variables = (string * Graphql_parser.const_value) list
  type execution_order = Serial | Parallel
  type 'ctx execution_context = {
    variables : variable_map;
    fragments : fragment_map;
    ctx       : 'ctx;
  }

  type path = [`String of string | `Int of int] list
  type error = field_error * path

  type resolve_error = [
    | `Resolve_error of error
    | `Argument_error of string
    | `Validation_error of string
  ]

  type execute_error = [
    resolve_error
    | `Mutations_not_configured
    | `Subscriptions_not_configured
    | `No_operation_found
    | `Operation_name_required
    | `Operation_not_found
  ]

  type 'a response = ('a, json) result

  let matches_type_condition type_condition (obj : ('ctx, 'src) obj) =
    obj.name = type_condition ||
      List.exists (fun (abstract : abstract) -> abstract.name = type_condition) !(obj.abstracts)

  let rec should_include_field ctx (directives : Graphql_parser.directive list) =
    match directives with
    | [] -> Ok true
    | { name = "skip"; arguments }::rest ->
      eval_directive ctx skip_directive arguments rest
    | { name = "include"; arguments }::rest ->
      eval_directive ctx include_directive arguments rest
    | { name; _ }::_ ->
        let err = Format.sprintf "Unknown directive: %s" name in
        Error err

  and eval_directive ctx (Directive { name; args; resolve; _ }) arguments rest =
    let open Rresult in
    Arg.eval_arglist ctx.variables ~field_type:"directive" ~field_name:name args arguments resolve >>= function
      | `Skip -> Ok false
      | `Include -> should_include_field ctx rest

  let alias_or_name : Graphql_parser.field -> string = fun field ->
    match field.alias with
    | Some alias -> alias
    | None -> field.name

  let rec merge_selections ?(memo=[]) = function
    | [] -> List.rev memo
    | field::fields ->
        let id = alias_or_name field in
        let matching, rest = List.partition (fun field' -> id = (alias_or_name field')) fields in
        let selection_sets = List.map (fun (field : Graphql_parser.field) -> field.selection_set) (field::matching) in
        let selection_set  = List.concat selection_sets in
        merge_selections ~memo:({field with selection_set}::memo) rest

  let rec collect_fields : 'ctx execution_context -> ('ctx, 'src) obj -> Graphql_parser.selection list -> (Graphql_parser.field list, string) result =
    fun ctx obj fields ->
    let open Rresult in
    List.map (function
    | Graphql_parser.Field field ->
        should_include_field ctx field.directives >>| fun include_field ->
          if include_field then [field] else []
    | Graphql_parser.FragmentSpread spread ->
        begin match StringMap.find spread.name ctx.fragments with
        | Some { directives; type_condition; selection_set; _ }
          when matches_type_condition type_condition obj ->
          should_include_field ctx directives >>= fun include_field ->
            if include_field then
              collect_fields ctx obj selection_set
            else Ok []
        | _ -> Ok []
        end
    | Graphql_parser.InlineFragment fragment ->
        let matches_type_condition = match fragment.type_condition with
          | None -> true
          | Some condition -> matches_type_condition condition obj
        in
        if matches_type_condition then
          should_include_field ctx fragment.directives >>= fun include_field ->
            if include_field then
              collect_fields ctx obj fragment.selection_set
            else Ok []
        else
          Ok []
    ) fields
    |> List.Result.join
    |> Rresult.R.map List.concat
    |> Rresult.R.map merge_selections

  let field_from_object : ('ctx, 'src) obj -> string -> ('ctx, 'src) field option = fun obj field_name ->
    List.find (fun (Field field) -> field.name = field_name) (Lazy.force obj.fields)

  let field_from_subscription_object = fun obj field_name ->
    List.find (fun (SubscriptionField field) -> field.name = field_name) obj.fields

  let coerce_or_null : 'a option -> ('a -> (json * error list, 'b) result Io.t) -> (json * error list, 'b) result Io.t =
    fun src f ->
      match src with
      | None -> Io.ok (`Null, [])
      | Some src' -> f src'

  let map_fields_with_order = function
    | Serial -> Io.map_s ~memo:[]
    | Parallel -> Io.map_p

  let error_to_json ?path ?extensions msg =
    let props = match path with
    | Some path -> ["path", `List (List.rev path :> json list)]
    | None -> []
    in
    let extension_props = match extensions with
    | None
    | Some [] -> []
    | Some extensions -> ["extensions", `Assoc extensions]
    in
    (`Assoc (("message", `String msg)::(List.append props extension_props)) : json)

  let error_response ?data ?path ?extensions msg =
    let errors = "errors", `List [
      error_to_json ?path ?extensions msg
    ]
    in
    let data = match data with
    | None -> []
    | Some data -> ["data", data]
    in
    `Assoc (errors :: data)

  let rec present : type ctx src. ctx execution_context -> src -> Graphql_parser.field -> (ctx, src) typ -> path -> (json * error list, [> resolve_error]) result Io.t =
    fun ctx src query_field typ path ->
      match typ with
      | Scalar s -> coerce_or_null src (fun x -> Io.ok (s.coerce x, []))
      | List t ->
          coerce_or_null src (fun src' ->
            List.mapi (fun i x -> present ctx x query_field t ((`Int i)::path)) src'
            |> Io.all
            |> Io.map ~f:List.Result.join
            |> Io.Result.map ~f:(fun xs -> (`List (List.map fst xs), List.map snd xs |> List.concat))
          )
      | NonNullable t -> present ctx (Some src) query_field t path
      | Object o ->
          coerce_or_null src (fun src' ->
            match collect_fields ctx o query_field.selection_set with
            | Ok fields -> resolve_fields ctx src' o fields path
            | Error e -> Io.error (`Argument_error e))
      | Enum e ->
          coerce_or_null src (fun src' ->
            match List.find (fun enum_value -> src' == enum_value.value) e.values with
            | Some enum_value -> Io.ok (`String enum_value.name, [])
            | None -> Io.ok (`Null, [])
          )
      | Abstract _ ->
          coerce_or_null src (fun (AbstractValue (typ', src')) ->
            present ctx (Some src') query_field typ' path
          )

  and resolve_field : type ctx src. ctx execution_context -> src -> Graphql_parser.field -> (ctx, src) field -> path -> ((string * json) * error list, [> resolve_error]) result Io.t =
    fun ctx src query_field (Field field) path ->
      let open Io.Infix in
      let name = alias_or_name query_field in
      let path' = (`String name)::path in
      let resolve_info = {
        ctx = ctx.ctx;
        field = query_field;
        fragments = ctx.fragments;
        variables = ctx.variables;
      } in
      let resolver = field.resolve resolve_info src in
      match Arg.eval_arglist ctx.variables ~field_name:field.name field.args query_field.arguments resolver with
      | Ok unlifted_value ->
          let lifted_value =
            field.lift unlifted_value
            |> Io.Result.map_error ~f:(fun err -> `Resolve_error (err, path')) >>=? fun resolved ->
            present ctx resolved query_field field.typ path'
          in
          lifted_value >>| (function
          | Ok (value, errors) ->
              Ok ((name, value), errors)
          | Error (`Argument_error _)
          | Error (`Validation_error _) as error ->
              error
          | Error (`Resolve_error err) as error ->
              match field.typ with
              | NonNullable _ ->
                  error
              | _ ->
                  Ok ((name, `Null), [err])
          )
      | Error err ->
          Io.error (`Argument_error err)

  and resolve_fields : type ctx src. ctx execution_context -> ?execution_order:execution_order -> src -> (ctx, src) obj -> Graphql_parser.field list -> path -> (json * error list, [> resolve_error]) result Io.t =
    fun ctx ?execution_order:(execution_order=Parallel) src obj fields path ->
      map_fields_with_order execution_order (fun (query_field : Graphql_parser.field) ->
        let name = alias_or_name query_field in
        if query_field.name = "__typename" then
          Io.ok ((name, `String obj.name), [])
        else
          match field_from_object obj query_field.name with
          | Some field ->
              resolve_field ctx src query_field field path
          | None ->
              let err = Printf.sprintf "Field '%s' is not defined on type '%s'" query_field.name obj.name in
              Io.error (`Validation_error err)
      ) fields
      |> Io.map ~f:List.Result.join
      |> Io.Result.map ~f:(fun xs -> (`Assoc (List.map fst xs), List.map snd xs |> List.concat))

  let data_to_json = function
    | data, [] -> `Assoc ["data", data]
    | data, errors ->
        let errors = List.map
          (fun (field_error, path) ->
            let extensions = Field_error.extensions_of_field_error field_error in
            let msg = Field_error.message_of_field_error field_error in
            error_to_json ~path ?extensions msg)
          errors
        in
        `Assoc [
          "errors", `List errors;
          "data", data;
        ]

  let to_response = function
    | Ok _ as res -> res
    | Error `No_operation_found ->
        Error (error_response "No operation found")
    | Error `Operation_not_found ->
        Error (error_response "Operation not found")
    | Error `Operation_name_required ->
        Error (error_response "Operation name required")
    | Error `Subscriptions_not_configured ->
        Error (error_response "Subscriptions not configured")
    | Error `Mutations_not_configured ->
        Error (error_response "Mutations not configured")
    | Error (`Validation_error msg) ->
        Error (error_response msg)
    | Error (`Argument_error msg) ->
        Error (error_response ~data:`Null msg)
    | Error (`Resolve_error (field_error, path)) ->
        let extensions = Field_error.extensions_of_field_error field_error in
        let msg = Field_error.message_of_field_error field_error in
        Error (error_response ~data:`Null ~path ?extensions msg)

  let subscribe : type ctx. ctx execution_context -> ctx subscription_field -> Graphql_parser.field -> (json response Io.Stream.t, [> resolve_error]) result Io.t
  =
    fun ctx (SubscriptionField subs_field) field ->
      let open Io.Infix in
      let name = alias_or_name field in
      let path = [`String name] in
      let resolve_info = {
        ctx = ctx.ctx;
        field;
        fragments = ctx.fragments;
        variables = ctx.variables
      } in
      let resolver = subs_field.resolve resolve_info in
      match Arg.eval_arglist ctx.variables ~field_name:subs_field.name subs_field.args field.arguments resolver with
      | Ok result ->
          result
          |> Io.Result.map ~f:(fun source_stream ->
            Io.Stream.map source_stream (fun value ->
              present ctx value field subs_field.typ path
              |> Io.Result.map ~f:(fun (data, errors) ->
                data_to_json (`Assoc [name, data], errors)
              )
              >>| to_response
            )
          )
          |> Io.Result.map_error ~f:(fun err ->
            `Resolve_error (err, path)
          )
      | Error err -> Io.error (`Argument_error err)

  let execute_operation : 'ctx schema -> 'ctx execution_context -> Graphql_parser.operation -> ([ `Response of json | `Stream of json response Io.Stream.t], [> execute_error]) result Io.t =
    fun schema ctx operation ->
      let open Io.Infix in
      match operation.optype with
      | Graphql_parser.Query ->
          let query  = schema.query in
          Io.return (collect_fields ctx query operation.selection_set)
          |> Io.Result.map_error ~f:(fun e -> `Argument_error e) >>=? fun fields ->
          (resolve_fields ctx () query fields [] : (json * error list, resolve_error) result Io.t :> (json * error list, [> execute_error]) result Io.t)
          |> Io.Result.map ~f:(fun data_errs -> `Response (data_to_json data_errs))
      | Graphql_parser.Mutation ->
          begin match schema.mutation with
          | None -> Io.error `Mutations_not_configured
          | Some mut ->
              Io.return (collect_fields ctx mut operation.selection_set)
              |> Io.Result.map_error ~f:(fun e -> `Argument_error e) >>=? fun fields ->
              (resolve_fields ~execution_order:Serial ctx () mut fields [] : (json * error list, resolve_error) result Io.t :> (json * error list, [> execute_error]) result Io.t)
              |> Io.Result.map ~f:(fun data_errs -> `Response (data_to_json data_errs))
          end
      | Graphql_parser.Subscription ->
          begin match schema.subscription with
          | None -> Io.error `Subscriptions_not_configured
          | Some subs ->
              Io.return (collect_fields ctx (obj_of_subscription_obj subs) operation.selection_set)
              |> Io.Result.map_error ~f:(fun e -> `Argument_error e) >>=? fun fields ->
              begin match fields with
              | [field] ->
                  (match field_from_subscription_object subs field.name with
                   | Some subscription_field ->
                       (subscribe ctx subscription_field field : ((json, json) result Io.Stream.t, resolve_error) result Io.t :> ((json, json) result Io.Stream.t, [> execute_error]) result Io.t)
                       |> Io.Result.map ~f:(fun stream -> `Stream stream)
                   | None -> Io.ok (`Response (`Assoc [(alias_or_name field, `Null)])))
              (* see http://facebook.github.io/graphql/June2018/#sec-Response-root-field *)
              | _ -> Io.error (`Validation_error "Subscriptions only allow exactly one selection for the operation.")
              end
          end

  let collect_fragments doc =
    List.fold_left (fun memo -> function
      | Graphql_parser.Operation _ -> memo
      | Graphql_parser.Fragment f -> StringMap.add f.name f memo
    ) StringMap.empty doc

  exception FragmentCycle of string list
  let rec validate_fragments fragment_map =
    try
      StringMap.iter (fun name _ ->
        validate_fragment fragment_map StringSet.empty name
      ) fragment_map;
      Ok fragment_map
    with FragmentCycle fragment_names ->
      let cycle = String.concat ", " fragment_names in
      let err = Format.sprintf "Fragment cycle detected: %s" cycle in
      Error (`Validation_error err)

  and validate_fragment (fragment_map : fragment_map) visited name =
    match StringMap.find name fragment_map with
    | None -> ()
    | Some fragment when StringSet.mem fragment.name visited ->
        raise (FragmentCycle (StringSet.elements visited))
    | Some fragment ->
        let visited' = StringSet.add fragment.name visited in
        List.iter (validate_fragment_selection fragment_map visited') fragment.selection_set

  and validate_fragment_selection fragment_map visited selection =
    match selection with
    | Graphql_parser.Field field ->
        List.iter (validate_fragment_selection fragment_map visited) field.selection_set
    | InlineFragment inline_fragment ->
        List.iter (validate_fragment_selection fragment_map visited) inline_fragment.selection_set
    | FragmentSpread fragment_spread ->
        validate_fragment fragment_map visited fragment_spread.name

  let collect_and_validate_fragments doc =
    let fragments = collect_fragments doc in
    validate_fragments fragments

  let collect_operations doc =
    List.fold_left (fun memo -> function
      | Graphql_parser.Operation op -> op::memo
      | Graphql_parser.Fragment _ -> memo
    ) [] doc

  let select_operation ?operation_name doc =
    let operations = collect_operations doc in
    match operation_name, operations with
    | _, [] -> Error `No_operation_found
    | None, [op] -> Ok op
    | None, _::_ -> Error `Operation_name_required
    | Some name, ops ->
        try
          Ok (List.find_exn (fun op -> op.Graphql_parser.name = Some name) ops)
        with Not_found ->
          Error `Operation_not_found

  let execute schema ctx ?variables:(variables=[]) ?operation_name doc =
    let open Io.Infix in
    let execute' schema ctx doc =
      Io.return (collect_and_validate_fragments doc) >>=? fun fragments ->
      let schema' = Introspection.add_built_in_fields schema in
      Io.return (select_operation ?operation_name doc) >>=? fun op ->
      let default_variables = List.fold_left (fun memo { Graphql_parser.name; default_value; _ } ->
        match default_value with
        | None -> memo
        | Some value -> StringMap.add name value memo
      ) StringMap.empty op.variable_definitions in
      let variables = List.fold_left (fun memo (name, value) -> StringMap.add name value memo) default_variables variables in
      let execution_ctx = { fragments; ctx; variables } in
      execute_operation schema' execution_ctx op
    in
    execute' schema ctx doc >>| to_response
end
