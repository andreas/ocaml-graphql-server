let id : 'a. 'a -> 'a = fun x -> x

let yojson = Alcotest.of_pp (
  fun formatter t ->
    Format.pp_print_text formatter (Yojson.Basic.pretty_to_string t)
)