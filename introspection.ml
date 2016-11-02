open Schema

(* ityp hides the 'src parameter to avoid scope escaping errors *)
type ityp = ITyp : ('ctx, 'src) typ -> ityp

(* ifield hides the 'src parameter to avoid scope escaping errors *)
type ifield = IField : ('ctx, 'src) field -> ifield

(* Extracts all types contained by a single type *)
let rec types : type src. ityp list -> ('ctx, src) typ -> ityp list = fun memo typ -> match typ with
  | List typ -> types memo typ
  | Nullable typ -> types memo typ
  | Scalar _ as scalar -> (ITyp scalar)::memo 
  | Enum _ as enum -> (ITyp enum)::memo
  | Object o as obj ->
      let memo'   = ((ITyp obj)::memo) in
      let reducer = fun memo (Schema.Field f) -> types memo f.typ in
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
      typ = string;
      resolve = fun _ name -> name
    };
    Field {
      name = "description";
      typ = Nullable string;
      resolve = fun _ e -> None
    };
    Field {
      name = "isDeprecated";
      typ = bool;
      resolve = fun _ e -> false
    };
    Field {
      name = "deprecationReason";
      typ = Nullable string;
      resolve = fun _ e -> None
    }
  ]
}

let __input_value = Object {
  name = "__InputValue";
  fields = []
}

let rec __type : 'ctx. ('ctx, ityp) typ = Object {
  name = "__Type";
  fields = [
    Field {
      name = "kind";
      typ = __type_kind;
      resolve = fun _ (ITyp t) -> match t with
        | Object _  -> `Object
        | List _    -> `List
        | Scalar _  -> `Scalar
        | Enum _    -> `Enum
        | Nullable _ -> `Nullable
    };
    Field {
      name = "name";
      typ = Nullable string;
      resolve = fun _ (ITyp t) -> match t with
        | Object o -> Some o.name
        | Scalar s -> Some s.name
        | Enum e -> Some e.name
        | _ -> None;
    };
    Field {
      name = "description";
      typ = Nullable string;
      resolve = fun _ t -> None;
    };
    Field {
      name = "fields";
      typ = Nullable (List __field);
      resolve = fun _ (ITyp t) -> match t with
        | Object o -> Some (List.map (fun f -> IField f) o.fields)
        | _ -> None
    };
    Field {
      name = "interfaces";
      typ = Nullable (List __type);
      resolve = fun _ (ITyp t) -> match t with
        | Object _ -> Some []
        | _ -> None
    };
    Field {
      name = "possibleTypes";
      typ = Nullable (List __type);
      resolve = fun _ t -> None
    };
    Field {
      name = "ofType";
      typ = Nullable __type;
      resolve = fun _ (ITyp t) -> match t with
        | Nullable typ -> Some (ITyp typ)
        | List typ     -> Some (ITyp typ)
        | _        -> None
    };
    Field {
      name = "enumValues";
      typ = Nullable (List __enum_value);
      resolve = fun _ (ITyp t) -> match t with
        | Enum e -> Some (List.map snd e.values)
        | _      -> None
    }
  ]
}

and __field : 'ctx. ('ctx, ifield) typ = Object {
  name = "__Field";
  fields = [
    Field {
      name = "name";
      typ = string;
      resolve = fun _ (IField (Field f)) -> f.name
    };
    Field {
      name = "description";
      typ = Nullable string;
      resolve = fun _ f -> None
    };
    Field {
      name = "args";
      typ = List __input_value;
      resolve = fun _ f -> [];
    };
    Field {
      name = "type";
      typ = __type;
      resolve = fun _ (IField (Field f)) -> ITyp f.typ
    };
    Field {
      name = "isDeprecated";
      typ = bool;
      resolve = fun _ f -> false
    };
    Field {
      name = "deprecationReason";
      typ = Nullable string;
      resolve = fun _ f -> None
    }
  ]
}

let __directive = Object {
  name = "__Directive";
  fields = [
    Field {
      name = "name";
      typ = string;
      resolve = fun _ d -> d.name
    }
  ]
}

let __schema : 'ctx. ('ctx, 'ctx schema) typ = Object {
  name = "__Schema";
  fields = [
    Field {
      name = "types";
      typ = List __type;
      resolve = fun _ s -> types [] (Object s.query);
    };
    Field {
      name = "queryType";
      typ = __type;
      resolve = fun _ s -> ITyp (Object s.query);
    };
    Field {
      name = "mutationType";
      typ = Nullable __type;
      resolve = fun _ s -> None;
    };
    Field {
      name = "directives";
      typ = List __directive;
      resolve = fun _ s -> []
    }
  ]
}

let add_schema_field s =
  let schema_field = Field {
    name = "__schema";
    typ = __schema;
    resolve = fun _ _ -> s
  } in
  { query = { s.query with fields = schema_field::s.query.fields } }
