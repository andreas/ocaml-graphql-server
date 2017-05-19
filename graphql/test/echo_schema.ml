open Graphql

let echo : 'a. unit -> unit -> 'a -> 'a = fun _ _ x -> x

let echo_field name field_typ arg_typ = Schema.(
      field name
            ~typ:field_typ
            ~args:Arg.[
              arg "x" ~typ:arg_typ
            ]
            ~resolve:echo
)

type colors = Red | Green | Blue
let color_enum     = Schema.enum "color" ~values:[Red, "RED"; Green, "GREEN"; Blue, "BLUE"]
let color_enum_arg = Schema.Arg.enum "color" ~values:["RED", Red; "GREEN", Green; "BLUE", Blue]

let person_arg = Schema.Arg.(obj "person" ~fields:Arg.[
    arg "title" ~typ:string;
    arg "first_name" ~typ:(non_null string);
    arg "last_name" ~typ:(non_null string);
  ]
  ~coerce:(fun title first last -> (title, first, last))
)

let schema =
  Schema.(schema [
      echo_field "string" string Arg.string;
      echo_field "float" float Arg.float;
      echo_field "int" int Arg.int;
      echo_field "bool" bool Arg.bool;
      echo_field "enum" color_enum color_enum_arg;
      echo_field "id" guid Arg.guid;
      echo_field "bool_list" (list bool) Arg.(list bool);
      field "input_obj"
            ~typ:(non_null string)
            ~args:Arg.[
              arg "x" ~typ:(non_null person_arg)
            ]
            ~resolve:(fun () () (title, first, last) -> first ^ " " ^ last)
            ;
      field "sum_defaults"
            ~typ:int
            ~args:Arg.[
              arg' "x" ~typ:string ~default:"42";
              arg' "y" ~typ:int ~default:3
            ]
            ~resolve:(fun () () x y -> try Some ((int_of_string x) + y) with _ -> None)
  ])