# `ocaml-graphql-server`

`ocaml-graphql-server` is a library for creating GraphQL servers. It's currently in a very early, experimental stage.

Current feature set:

- [x] GraphQL parser in pure OCaml using [angstrom](https://github.com/inhabitedtype/angstrom) (April 2016 RFC draft)
- [x] Basic execution
- [x] Basic introspection
- [x] Example with HTTP server and GraphiQL

![GraphiQL Example](https://cloud.githubusercontent.com/assets/2518/20041922/403ed918-a471-11e6-8178-1cd22dbc4658.png)

## Documentation

[API documentation](https://andreas.github.io/ocaml-graphql-server/)

## Examples

### GraphiQL

To run a sample GraphQL server also serving GraphiQL, do the following:

```bash
git checkout git@github.com/andreas/ocaml-graphql-server.git
cd ocaml-graphql-server
opam pin add graphql-server .
cd examples
ocamlbuild -use-ocamlfind server.native && ./server.native
```

Now open [http://localhost:8080](http://localhost:8080).

### Defining a Schema

```ocaml
open Graphql

type user = {
  id   : int;
  name : string;
  role : [`user | `admin];
}

let users = [
  { id = 1; name = "Alice"; role = `admin };
  { id = 2; name = "Bob"; role = `user }
]

let role = Schema.enum
  ~name:"role"
  ~values:[(`user, "user"); (`admin, "admin")]

let user = Schema.(obj
  ~name:"user"
  ~fields:[
    field
      ~name:"id"
      ~typ:(non_null int)
      ~resolve:(fun () p -> p.id)
    ;
    field
      ~name:"name"
      ~typ:(non_null string)
      ~resolve:(fun () p -> p.name)
    ;
    field
      ~name:"role"
      ~typ:(non_null role)
      ~resolve:(fun () p -> p.role)
  ]
)

let schema = Schema.(schema 
    ~fields:[
      field
        ~name:"users"
        ~typ:(non_null (list (non_null user)))
        ~resolve:(fun () () -> users)
    ]
)
```

### Running a Query

```ocaml
let query = Graphql.Parser.parse some_string in
Graphql.execute schema ctx query
```

## Design

Only valid schemas should pass the type checker. If a schema compiles, the following should hold:

1. The type of a field should agree with the return type of the resolver function.
2. The source of a field should agree with the type of the object to which it belongs.
3. The context argument for all resolver functions in a schema should agree.

The following types ensure this:

```ocaml
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
```

The type parameters can be interpreted as follows:

- `'ctx` is a value that is passed all resolvers when executing a query against a schema,
- ``src` is the domain-specific value, e.g. a user record,
- `'a` is the result of the resolver, which must agree with the type of the field. 

(note that arguments are currently not handled)

Particularly noteworthy is `('ctx, 'src) field`, which hides the type `'a`. The type `'a` is used to ensure that the output of a resolver function agrees with the input type of the fields type.

For introspection, two additional types are used to hide the type parameters `'ctx` and `src`:

```ocaml
type ityp = ITyp : ('ctx, 'src) typ -> ityp
type ifield = IField : ('ctx, 'src) field -> ifield
```

This is to avoid "type parameter would avoid it's scope"-errors.
