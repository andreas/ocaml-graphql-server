open Schema

type person = {
  name : string;
  age : int;
  gender : [`male | `female];
}

let gender = Enum {
  name = "gender";
  values = [(`male, "male"); (`female, "female")];
}

let person : (unit, person) typ = Object {
  name = "Person";
  fields = [
    Field {
      name = "name";
      typ = string;
      resolve = fun () p -> p.name;
    };
    Field {
      name = "age";
      typ = int;
      resolve = fun () p -> p.age;
    };
    Field {
      name = "job_title";
      typ = Nullable string;
      resolve = fun () p -> Some "foo";
    };
    Field {
      name = "gender";
      typ = gender;
      resolve = fun () p -> p.gender
    }
  ];
}

let query : (unit, unit) obj = {
  name = "root";
  fields = [
    Field {
      name = "user";
      typ = List person;
      resolve = fun () () -> [{ name = "John Doe"; age = 42; gender = `male }]
    };
  ]
}

let schema = {
  query;
}

let read_all () =
  let lines = ref [] in
  try
    while true do
      lines := read_line () :: !lines
    done;
    ""
  with End_of_file -> String.concat "\n" (List.rev !lines)

let () =
  let query = read_all () in
  print_endline query;
  match Angstrom.(parse_only (Gqlparser.document <* end_of_input) (`String query)) with
  | Ok doc ->
      Execute.execute schema doc ()
      |> Yojson.Basic.to_string
      |> print_endline
  | Ok _ -> print_endline "ERR: expected operation"
  | Error err -> print_endline (Format.sprintf "ERR: %s" err)
