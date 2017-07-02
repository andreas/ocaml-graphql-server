GraphQL Servers in OCaml
-----------------------------------------------

![Build Status](https://travis-ci.org/andreas/ocaml-graphql-server.svg?branch=master)

This repo contains a library for creating GraphQL servers in OCaml. Note that the API is still under active development.

Current feature set:

- [x] Type-safe schema design
- [x] GraphQL parser in pure OCaml using [angstrom](https://github.com/inhabitedtype/angstrom) (April 2016 RFC draft)
- [x] Query execution
- [x] Introspection of schemas
- [x] Arguments for fields
- [x] Allows variables in queries
- [x] Lwt support
- [x] Async support
- [x] Example with HTTP server and GraphiQL

![GraphiQL Example](https://cloud.githubusercontent.com/assets/2518/22173954/8d1e5bbe-dfd1-11e6-9a7e-4f93d0ce2e24.png)

## Documentation

Three OPAM packages are provided:

- `graphql` provides the core functionality and is IO-agnostic. It provides a functor `Graphql.Schema.Make(IO)` to instantiate with your own IO monad.
- `graphql-lwt` provides the module `Graphql_lwt.Schema` with [Lwt](https://github.com/ocsigen/lwt) support in field resolvers.
- `graphql-async` provides the module `Graphql_async.Schema` with [Async](https://github.com/janestreet/async) support in field resolvers.`

API documentation:

- [`graphql`](https://andreas.github.io/ocaml-graphql-server/graphql)
- [`graphql-lwt`](https://andreas.github.io/ocaml-graphql-server/graphql-lwt)
- [`graphql-async`](https://andreas.github.io/ocaml-graphql-server/graphql-async)

## Examples

### GraphiQL

To run a sample GraphQL server also serving GraphiQL, do the following:

```bash
opam install graphql-lwt jbuilder
git checkout git@github.com/andreas/ocaml-graphql-server.git
cd ocaml-graphql-server
cd examples
jbuilder build server.exe
./_build/default/server.exe
```

Now open [http://localhost:8080](http://localhost:8080).

### Defining a Schema

```ocaml
open Graphql

type role = User | Admin
type user = {
  id   : int;
  name : string;
  role : role;
}

let users = [
  { id = 1; name = "Alice"; role = Admin };
  { id = 2; name = "Bob"; role = User }
]

let role = Schema.(enum "role"
  ~doc:"The role of a user"
  ~values:[
    enum_value "USER" ~value:User;
    enum_value "ADMIN" ~value:Admin;
  ]
)

let user = Schema.(obj "user"
  ~doc:"A user in the system"
  ~fields:(fun _ -> [
    field "id"
      ~doc:"Unique user identifier"
      ~typ:(non_null int)
      ~args:Arg.[]
      ~resolve:(fun () p -> p.id)
    ;
    field "name"
      ~typ:(non_null string)
      ~args:Arg.[]
      ~resolve:(fun () p -> p.name)
    ;
    field "role"
      ~typ:(non_null role)
      ~args:Arg.[]
      ~resolve:(fun () p -> p.role)
  ])
)

let schema = Schema.(schema [
  field "users"
    ~typ:(non_null (list (non_null user)))
    ~args:Arg.[]
    ~resolve:(fun () () -> users)
])
```

### Running a Query

Without variables:

```ocaml
let query = Graphql_parser.parse "{ users { name } }" in
Graphql.Schema.execute schema ctx query
```

With variables parsed from JSON:

```ocaml
let query = Graphql_parser.parse "{ users(limit: $x) { name } }" in
let json_variables = Yojson.Basic.(from_string "{\"x\": 42}" |> Util.to_assoc) in
let variables = (json_variables :> (string * Graphql_parser.const_value) list)
Graphql.Schema.execute schema ctx ~variables query
```

### Self-Recursive Objects

To allow defining an object that refers to itself, the type itself is provided as argument to the `~fields` function. Example:

```ocaml
type tweet = {
  id : int;
  replies : tweet list;
}

let tweet = Schema.(obj "tweet"
  ~fields:(fun tweet -> [
    field "id"
      ~typ:(non_null int)
      ~args:Arg.[]
      ~resolver:(fun ctx t -> t.id)
    ;
    field "replies"
      ~typ:(non_null (list tweet))
      ~args:Arg.[]
      ~resolver:(fun ctx t -> t.replies)
  ])
)
```

### Mutually Recursive Objects

Mutually recursive objects can be defined using `let rec` and `lazy`:

```ocaml
let rec foo = lazy Schema.(obj "foo"
  ~fields:(fun _ -> [
    field "bar"
      ~typ:Lazy.(force bar)
      ~args.Arg.[]
      ~resolver:(fun ctx foo -> foo.bar)
  ])
and bar = lazy Schema.(obj "bar"
  ~fields:(fun _ -> [
    field "foo"
      ~typ:Lazy.(force foo)
      ~args.Arg.[]
      ~resolver:(fun ctx bar -> bar.foo)
  ])
```

### Lwt Support

```ocaml
open Lwt.Infix
open Graphql_lwt

let schema = Schema.(schema [
  io_field "wait"
    ~typ:(non_null float)
    ~args:Arg.[
      arg "duration" ~typ:float;
    ]
    ~resolve:(fun () () -> Lwt_unix.sleep duration >|= fun () -> duration)
])
```

### Async Support

```ocaml
open Core.Std
open Async.Std
open Graphql_async

let schema = Schema.(schema [
  io_field "wait"
    ~typ:(non_null float)
    ~args:Arg.[
      arg "duration" ~typ:float;
    ]
    ~resolve:(fun () () -> after (Time.Span.of_float duration) >>| fun () -> duration)
])
```

### Arguments

Arguments for a field can either be required, optional or optional with a default value:

```ocaml
Schema.(obj "math"
  ~fields:(fun _ -> [
    field "sum"
      ~typ:int
      ~args:Arg.[
        arg  "x" ~typ:(non_null int); (* <-- required *)
        arg  "y" ~typ:int;            (* <-- optional *)
        arg' "z" ~typ:int ~default:7  (* <-- optional w/ default *)
      ]
      ~resolve:(fun x y z ->
        let y' = match y with Some n -> n | None -> 42 in
        x + y' + z
      )
  ])
)
```

Note that you must use `arg'` to provide a default value.

## Design

Only valid schemas should pass the type checker. If a schema compiles, the following holds:

1. The type of a field agrees with the return type of the resolve function.
2. The arguments of a field agrees with the accepted arguments of the resolve function.
3. The source of a field agrees with the type of the object to which it belongs.
4. The context argument for all resolver functions in a schema agree.

The following types ensure this:

```ocaml
type ('ctx, 'src) obj = {
  name   : string;
  fields : ('ctx, 'src) field list Lazy.t;
}
and ('ctx, 'src) field =
  Field : {
    name    : string;
    typ     : ('ctx, 'out) typ;
    args    : ('out, 'args) Arg.arg_list;
    resolve : 'ctx -> 'src -> 'args;
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
- `'src` is the domain-specific source value, e.g. a user record,
- `'args` is the arguments of the resolver, and will be of the type `'arg¹ -> ... -> 'argⁿ -> 'out`,
- `'out` is the result of the resolver, which must agree with the type of the field.

Particularly noteworthy is `('ctx, 'src) field`, which hides the type `'out`. The type `'out` is used to ensure that the output of a resolver function agrees with the input type of the field's type.

For introspection, three additional types are used to hide the type parameters `'ctx` and `src`:

```ocaml
  type any_typ =
    | AnyTyp : (_, _) typ -> any_typ
    | AnyArgTyp : (_, _) Arg.arg_typ -> any_typ
  type any_field =
    | AnyField : (_, _) field -> any_field
    | AnyArgField : (_, _) Arg.arg -> any_field
  type any_arg = AnyArg : (_, _) Arg.arg -> any_ar
```

This is to avoid "type parameter would avoid it's scope"-errors.
