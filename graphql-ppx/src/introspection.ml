type enum_value = {
    name : string;
}

type field = {
    name : string;
    args : input_value list;
    typ : type_ref;
}

and typ =
    | Object of {
        name : string;
        fields : field list;
        interfaces : string list;
      }
    | Scalar of {
        name : string;
      }
    | Interface of {
        name : string;
        fields : field list;
        possible_types : string list;
      }
    | Union of {
        name : string;
        possible_types : string list;
      }
    | Enum of {
        name : string;
        enum_values : enum_value list;
      }
    | InputObject of {
        name : string;
        input_fields : input_value list;
      }

and input_value = {
    name : string;
    typ : type_ref;
    default_value : string option;
}

and type_ref =
  | List of type_ref
  | NonNull of type_ref
  | Type of string

type schema = {
    query_type : string;
    mutation_type : string option;
    types : typ list;
}

let named_of_yojson json =
    Yojson.Basic.Util.(json |> member "name" |> to_string)

let enum_value_of_yojson json =
    let open Yojson.Basic.Util in
    {
        name = json |> member "name" |> to_string;
    }

let rec field_of_yojson json =
    let open Yojson.Basic.Util in
    {
        name = json |> member "name" |> to_string;
        args = json |> member "args" |> convert_each input_value_of_yojson;
        typ = json |> member "type" |> type_ref_of_yojson;
    }

and typ_of_yojson json =
    let open Yojson.Basic.Util in
    let name = json |> member "name" |> to_string in
    let kind = json |> member "kind" |> to_string in
    match kind with
    | "SCALAR" ->
        Scalar {
            name;
        }
    | "OBJECT" ->
        Object {
            name;
            fields = json |> member "fields" |> convert_each field_of_yojson;
            interfaces = json |> member "interfaces" |> convert_each named_of_yojson;
        }
    | "INTERFACE" ->
        Interface {
            name;
            fields = json |> member "fields" |> convert_each field_of_yojson;
            possible_types = json |> member "possibleTypes" |> convert_each named_of_yojson;
        }
    | "UNION" ->
        Union {
            name;
            possible_types = json |> member "possibleTypes" |> convert_each named_of_yojson;
        }
    | "ENUM" ->
        Enum {
            name;
            enum_values = json |> member "enumValues" |> convert_each enum_value_of_yojson;
        }
    | "INPUT_OBJECT" ->
        InputObject {
            name;
            input_fields = json |> member "inputFields" |> convert_each input_value_of_yojson;
        }
    | _ -> failwith (Format.sprintf "Unknown kind `%s`" kind)

and input_value_of_yojson json =
    let open Yojson.Basic.Util in
    {
        name = json |> member "name" |> to_string;
        typ = json |> member "type" |> type_ref_of_yojson;
        default_value = json |> member "defaultValue" |> to_string_option;
    }

and type_ref_of_yojson json = 
    let open Yojson.Basic.Util in
    let kind = json |> member "kind" |> to_string in
    match kind with
    | "SCALAR"
    | "OBJECT"
    | "INTERFACE"
    | "UNION"
    | "ENUM"
    | "INPUT_OBJECT" ->
        let name = json |> member "name" |> to_string in
        Type name
    | "LIST" ->
        let of_type = json |> member "ofType" |> type_ref_of_yojson in
        List of_type
    | "NON_NULL" ->
        let of_type = json |> member "ofType" |> type_ref_of_yojson in
        NonNull of_type
    | _ -> failwith (Format.sprintf "Unknown kind `%s`" kind)

let schema_of_yojson json =
    let open Yojson.Basic.Util in
    {
        query_type = json |> member "queryType" |> named_of_yojson;
        mutation_type = json |> member "mutationType" |> to_option (named_of_yojson);
        types = json |> member "types" |> convert_each typ_of_yojson;
    }

let of_file f =
    let chan = open_in f in
    let json = Yojson.Basic.from_channel chan in
    Yojson.Basic.Util.(json |> member "data" |> member "__schema" |> schema_of_yojson)