open Graphql

type cat = {
  name : string;
  kittens : int;
}

type dog = {
  name : string;
  puppies : int;
}

let meow : cat = { name = "Meow"; kittens = 1 }
let fido : dog = { name = "Fido"; puppies = 2 }

let cat = Schema.(obj "Cat"
  ~fields:(fun _ -> [
    field "name"
      ~typ:(non_null string)
      ~args:Arg.[]
      ~resolve:(fun () (cat : cat) -> cat.name)
    ;
    field "kittens"
      ~typ:(non_null int)
      ~args:Arg.[]
      ~resolve:(fun () (cat : cat) -> cat.kittens)
    ;
  ])
)

let dog = Schema.(obj "Dog"
  ~fields:(fun _ -> [
    field "name"
      ~typ:(non_null string)
      ~args:Arg.[]
      ~resolve:(fun () (dog : dog) -> dog.name)
    ;
    field "puppies"
      ~typ:(non_null int)
      ~args:Arg.[]
      ~resolve:(fun () (dog : dog) -> dog.puppies)
    ;
  ])
)

let pet : (unit, [`pet]) Schema.abstract_typ = Schema.union "Pet"
let cat_as_pet = Schema.add_type pet cat
let dog_as_pet = Schema.add_type pet dog

let named : (unit, [`named]) Schema.abstract_typ = Schema.(interface "Named"
  ~fields:(fun _ -> [
    abstract_field "name"
      ~typ:(non_null string)
      ~args:Arg.[]
  ])
)
let cat_as_named = Schema.add_type named cat
let dog_as_named = Schema.add_type named dog

let pet_type = Schema.(Arg.enum "pet_type"
  ~values:[
    enum_value "CAT" ~value:`Cat;
    enum_value "DOG" ~value:`Dog;
  ])

let schema = Schema.(schema [
    field "pet"
      ~typ:(non_null pet)
      ~args:Arg.[
        arg "type" ~typ:(non_null pet_type)
      ]
      ~resolve:(fun () () pet_type ->
        match pet_type with
        | `Cat ->
          cat_as_pet meow
        | `Dog ->
          dog_as_pet fido
      )
    ;
    field "pets"
      ~typ:(non_null (list (non_null pet)))
      ~args:Arg.[]
      ~resolve:(fun () () -> [cat_as_pet meow; dog_as_pet fido])
    ;
    field "named_objects"
      ~typ:(non_null (list (non_null named)))
      ~args:Arg.[]
      ~resolve:(fun () () -> [cat_as_named meow; dog_as_named fido])
  ])

let test_query = Test_common.test_query schema ()

let suite = [
  ("dog as pet", `Quick, fun () ->
    let query = "{ pet(type: \"DOG\") { ... on Dog { name puppies } } }" in
    test_query query (`Assoc [
			"data", `Assoc [
				"pet", `Assoc [
					"name", `String "Fido";
					"puppies", `Int 2
				]
			]
		])
  );
  ("cat as pet", `Quick, fun () ->
    let query = "{ pet(type: \"CAT\") { ... on Cat { name kittens } } }" in
    test_query query (`Assoc [
			"data", `Assoc [
				"pet", `Assoc [
					"name", `String "Meow";
					"kittens", `Int 1
				]
			]
		])
  );
  ("pets", `Quick, fun () ->
    let query = "{ pets { ... on Dog { name puppies } ... on Cat { name kittens } } }" in
    test_query query (`Assoc [
			"data", `Assoc [
				"pets", `List [
					`Assoc [
						"name", `String "Meow";
						"kittens", `Int 1
					];
					`Assoc [
						"name", `String "Fido";
						"puppies", `Int 2
					]
				]
			]
		])
  );
  ("named_objects", `Quick, fun () ->
    let query = "{ named_objects { name ... on Dog { puppies } ... on Cat { kittens } } }" in
    test_query query (`Assoc [
			"data", `Assoc [
				"named_objects", `List [
					`Assoc [
						"name", `String "Meow";
						"kittens", `Int 1
					];
					`Assoc [
						"name", `String "Fido";
						"puppies", `Int 2
					]
				]
			]
		])
  );
]
