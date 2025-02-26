%token <string> METHOD
%token <string> HTTP_VERSION
%token <string> OTHER
%token <string> PATH
%token <string> HEADER_NAME
%token <string> HEADER_VALUE
%token CRLF EOF

%start main
%type <Ast.http_request_header> main

%%

main:
  request EOF { $1 }

request:
  request_line headers CRLF {
    (* build your request AST here *)
    {
      method_ = fst (fst $1);
      path    = snd (fst $1);
      version = snd $1;
      headers = $2;
    }
  }

request_line:
  METHOD PATH HTTP_VERSION CRLF {
    (* parse method, path, version *)
    (( $1, $2 ), $3)
  }

headers:
  | header headers { $1 :: $2 }
  | /* empty */ { [] }

header:
  HEADER_NAME HEADER_VALUE CRLF { ($1, $2) }
