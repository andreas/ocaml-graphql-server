%{
open Ast
%}

%token <string> NAME
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token COLON
%token EQUAL
%token BANG
%token ELLIPSIS
%token DOLLAR
%token AT
%token FRAGMENT
%token MUTATION
%token NULL
%token ON
%token QUERY
%token SUBSCRIPTION

%token EOF

%start doc
%type <Ast.document> doc

%%

doc:
  | definition+ EOF { $1 }

definition:
  | operation
  | fragment { $1 }

fragment:
  | FRAGMENT fragment_name type_condition directive* selection_set
    {
      Fragment {
        name = $2;
        type_condition = $3;
        directives = $4;
        selection_set = $5;
      }
    }

operation:
  | selection_set
    {
        Operation {
          optype = Query;
          name = None;
          variable_definitions = [];
          directives = [];
          selection_set = $1;
        }
      }
  | optype name? loption(variable_definitions) directive* selection_set
    {
      Operation {
        optype = $1;
        name = $2;
        variable_definitions = $3;
        directives = $4;
        selection_set = $5;
      }
    }

optype:
  | QUERY { Query }
  | MUTATION { Mutation }
  | SUBSCRIPTION { Subscription }

selection_set:
  | LBRACE selection+ RBRACE { $2 }

selection:
  | field
  | fragment_spread
  | inline_fragment { $1 }

field:
  | name COLON name loption(arguments) directive* loption(selection_set)
    {
      Field {
        alias = Some $1;
        name = $3;
        arguments = $4;
        directives = $5;
        selection_set = $6;
      }
    }
  | name loption(arguments) directive*; loption(selection_set)
    {
      Field {
        alias = None;
        name = $1;
        arguments = $2;
        directives = $3;
        selection_set = $4;
      }
    }

fragment_spread:
  | ELLIPSIS fragment_name directive*
    {
      FragmentSpread {
        name = $2;
        directives = $3;
      }
    }

type_condition:
  | ON name { $2 }

inline_fragment:
  | ELLIPSIS type_condition? directive* selection_set
    {
      InlineFragment {
        type_condition = $2;
        directives = $3;
        selection_set = $4;
      }
    }

variable_definitions:
  | LPAREN variable_definition* RPAREN { $2  }

default_value:
  | EQUAL const_value { $2 }

variable_definition:
  | DOLLAR name COLON typ default_value? {
    {
      name = $2;
      typ = $4;
      default_value = $5;
    }
  }

typ:
  | name { NamedType $1 }
  | LBRACK typ RBRACK { ListType $2 }
  | typ BANG { NonNullType $1 }

directive:
  | AT name loption(arguments)
    {
      {
        name = $2;
        arguments = $3;
      }
    }

arguments:
  | LPAREN argument* RPAREN { $2 }

argument:
  | name COLON value { $1, $3 }

value_parser(X):
  | NULL { `Null }
  | INT { `Int $1 }
  | FLOAT { `Float $1 }
  | STRING { `String $1 }
  | BOOL { `Bool $1 }
  | enum_value { `Enum $1 }
  | LBRACK X* RBRACK { `List $2 }
  | LBRACE list(name COLON X { $1, $3 }) RBRACE { `Assoc $2 }

value:
  | DOLLAR name { `Variable $2 }
  | value_parser(value) { $1 }

const_value:
  | value_parser(const_value) { $1 }

keyword_name:
  | QUERY { "query" }
  | MUTATION { "mutation" }
  | SUBSCRIPTION { "subscription" }
  | FRAGMENT { "fragment" }

enum_value:
  | keyword_name
  | NAME { $1 }

fragment_name:
  | NULL { "null" }
  | BOOL { string_of_bool $1 }
  | NAME
  | keyword_name { $1 }

name:
  | fragment_name { $1 }
  | ON { "on" }
