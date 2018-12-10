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
- [x] GraphQL Subscriptions

## Documentation

Four OPAM packages are provided:

- `graphql` provides the core functionality and is IO-agnostic. It provides a functor `Graphql.Schema.Make(IO)` to instantiate with your own IO monad.
- `graphql-lwt` provides the module `Graphql_lwt.Schema` with [Lwt](https://github.com/ocsigen/lwt) support in field resolvers.
- `graphql-async` provides the module `Graphql_async.Schema` with [Async](https://github.com/janestreet/async) support in field resolvers.
- `graphql_parser` provides query parsing functionality.
- `graphql-cohttp` allows exposing a schema over HTTP using [Cohttp](https://github.com/mirage/ocaml-cohttp).

API documentation:

- [`graphql`](https://andreas.github.io/ocaml-graphql-server/graphql)
- [`graphql-lwt`](https://andreas.github.io/ocaml-graphql-server/graphql-lwt)
- [`graphql-async`](https://andreas.github.io/ocaml-graphql-server/graphql-async)
- [`graphql_parser`](https://andreas.github.io/ocaml-graphql-server/graphql_parser)
- [`graphql-cohttp`](https://andreas.github.io/ocaml-graphql-server/graphql-cohttp)

## Examples

### GraphiQL

To run a sample GraphQL server also serving GraphiQL, do the following:

```bash
opam install graphql-lwt dune
git clone git@github.com:andreas/ocaml-graphql-server.git
cd ocaml-graphql-server/examples
dune exec ./server.exe
```

Now open [http://localhost:8080/graphql](http://localhost:8080/graphql).

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
      ~resolve:(fun info p -> p.id)
    ;
    field "name"
      ~typ:(non_null string)
      ~args:Arg.[]
      ~resolve:(fun info p -> p.name)
    ;
    field "role"
      ~typ:(non_null role)
      ~args:Arg.[]
      ~resolve:(fun info p -> p.role)
  ])
)

let schema = Schema.(schema [
  field "users"
    ~typ:(non_null (list (non_null user)))
    ~args:Arg.[]
    ~resolve:(fun info () -> users)
])
```

### Running a Query

Without variables:

```ocaml
match Graphql_parser.parse "{ users { name } }" with
| Ok query -> Graphql.Schema.execute schema ctx query
| Error err -> failwith err
```

With variables parsed from JSON:

```ocaml
match Graphql_parser.parse "{ users(limit: $x) { name } }" with
| Ok query ->
    let json_variables = Yojson.Basic.(from_string "{\"x\": 42}" |> Util.to_assoc) in
    let variables = (json_variables :> (string * Graphql_parser.const_value) list)
    Graphql.Schema.execute schema ctx ~variables query
| Error err ->
    failwith err
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
      ~resolve:(fun info t -> t.id)
    ;
    field "replies"
      ~typ:(non_null (list tweet))
      ~args:Arg.[]
      ~resolve:(fun info t -> t.replies)
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
      ~resolve:(fun info foo -> foo.bar)
  ])
and bar = lazy Schema.(obj "bar"
  ~fields:(fun _ -> [
    field "foo"
      ~typ:Lazy.(force foo)
      ~args.Arg.[]
      ~resolve:(fun info bar -> bar.foo)
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
    ~resolve:(fun info () ->
      Lwt_result.ok (Lwt_unix.sleep duration >|= fun () -> duration)
    )
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
    ~resolve:(fun info () ->
      after (Time.Span.of_float duration) >>| fun () -> duration
    )
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
      ~resolve:(fun info () x y z ->
        let y' = match y with Some n -> n | None -> 42 in
        x + y' + z
      )
  ])
)
```

Note that you must use `arg'` to provide a default value.

### Subscriptions

```ocaml
Schema.(schema [
     ...
  ]
  ~subscriptions:[
    subscription_field "user_created"
      ~typ:(non_null user)
      ~resolve:(fun info ->
        let user_stream, push_to_user_stream = Lwt_stream.create () in
        let destroy_stream = (fun () -> push_to_user_stream None) in
        Lwt_result.return (user_stream, destroy_stream))
    ])
```

### HTTP Server

Using Lwt:

```ocaml
open Graphql_lwt

let schema = Schema.(schema [
  ...
])

module Graphql_cohttp_lwt = Graphql_cohttp.Make (Schema) (Cohttp_lwt.Body)

let () =
  let callback = Graphql_cohttp_lwt.make_callback (fun _req -> ()) schema in
  let server = Cohttp_lwt_unix.Server.make ~callback () in
  let mode = `TCP (`Port 8080) in
  Cohttp_lwt_unix.Server.create ~mode server
  |> Lwt_main.run
```

## Design

Only valid schemas should pass the type checker. If a schema compiles, the following holds:

1. The type of a field agrees with the return type of the resolve function.
2. The arguments of a field agrees with the accepted arguments of the resolve function.
3. The source of a field agrees with the type of the object to which it belongs.
4. The context argument for all resolver functions in a schema agree.

The following blog posts introduces the core design concepts:

- https://andreas.github.io/2017/11/29/type-safe-graphql-with-ocaml-part-1/
- https://andreas.github.io/2018/01/05/modeling-graphql-type-modifiers-with-gadts/
