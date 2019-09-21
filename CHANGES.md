0.13.0 2019-09.21
---------------------------------

- Support for custom errors (#166)
- Add `__type` introspection field (#163)
- Better parameter handling for `graphql-cohttp` (#156)
- Fix use of default variables (#158)
- Fix merging of field selections (#174)

0.12.2 2019-04-02
---------------------------------

- Remove open of Result (#154)

0.12.1 2019-03-29
---------------------------------

- Disable Yojson deprecation warnings (#148)
- Fix routing in graphql-cohttp (#150)

0.12.0 2019-03-22
---------------------------------

- Remove Str from cohttp-graphql (#146)


0.11.0 2019-03-03
---------------------------------

- Minor parser improvements (#139)
- Replace Str with Re (#144)

0.10.0 2019-02-14
---------------------------------

- Fix digestif dependency in graphql-cohttp (#141)

0.9.0 2019-02-08
---------------------------------

- Skip and include directives (#117)
- Expose more data to resolvers (#127)
- Add the `errors` key first in the response JSON (#131)
- Rewrite parser to Menhir and replace sexp with fmt (#132)
- Support for websockets as transport (#133)

0.8.0 2018-12-06
---------------------------------

- Subscription support (#101)
- Add path to errors in response (#112)
- Improved escaped character handling (#114)
- Improve error messages for invalid arguments (#128)

0.7.0 2018-08-10
---------------------------------

- Allow returning errors from resolve function of io_field (#69)
- Support for union and interface types (#70)
- Expose HTTP request to context construction function in Graphql_lwt.Server.start (#88)
- Fix error response from Graphql_lwt.Server (#96)
- Allow passing operation name to Graphql_lwt.Server (#103)
- Querying undefined fields gives validation error (#105)
- Fix parsing of enums with E (#109)

0.6.0 2018-03-31
---------------------------------

- Prevent fragment cycles (#78)
- graphql-lwt depends on cohttp 1.0.0 (#71)

0.5.0 2018-01-18
---------------------------------

- Depend on angstrom 0.7.0 (#64)
- Fix parsing of quoted strings (#64)
- Make custom argument types generalizable (#72)
- Deduplicate arg types in introspection result (#75)
- 4.06 compatibility (#76)

0.4.0 2017-09-17
---------------------------------

- Parse tabs as whitespace (#62)
- Move parser to separate package (#63)

0.3.0 2017-08-31
---------------------------------

- Built-in HTTP server for graphql-lwt (#60)

0.2.0 2017-07-02
---------------------------------

- Support deprecation of fields (#53)
- Support documentation and deprecation of enum values (#54)

0.1.0 2017-05-25
---------------------------------

Initial public release.
