{
  (* The header of http_lexer.mll: we can open the parser's token types *)
  open Http_parser  (* from http_parser.mly, which will generate a module Http_parser *)
  let current_lexer = ref (fun _ -> failwith "Lexer not set yet")
}

rule start = parse
  |  "GET" | "POST"
    {
      METHOD (Lexing.lexeme lexbuf)
    }

  |  "HTTP/" ['0'-'9']+ '.' ['0'-'9']+
    {
      HTTP_VERSION (Lexing.lexeme lexbuf)
    }

  | '/' ['a'-'z' 'A'-'Z' '0'-'9' '_' '~' '!' '$' '&' '\'' '(' ')' '*' '+' ',' ';' '=' ':' '@' '%' '/' '-']*
    {
      PATH (Lexing.lexeme lexbuf)
    }

  | [' '  '\t' '\n']
    {
      start lexbuf
    }

  | "\r\n" | "\n"
    {
      current_lexer := (fun lb -> header lb);
      CRLF
    }

  | _ as c { raise (Failure ("Unexpected start character: " ^ String.make 1 c)) }

and header = parse
  |  ['A'-'Z' 'a'-'z' '0'-'9'] ['A'-'Z' 'a'-'z' '0'-'9' '-']*
    {
      HEADER_NAME (Lexing.lexeme lexbuf)
    }

  | ':' [' ' '\t']* [^'\n' '\r']*
    {
      HEADER_VALUE (Lexing.lexeme lexbuf)
    }

  | "\r\n" | "\n"
    {
      CRLF
    }

  | [' '  '\t' '\n'] { header lexbuf }  (* skip whitespace *)

  | eof
    {
      EOF
    }

  | _ as c { raise (Failure ("Unexpected header character: " ^ String.make 1 c)) }

(* Entry point of the lexer: parse request line, then switch to headers *)
{
  current_lexer := (fun lb -> start lb)
  let read_token (lexbuf : Lexing.lexbuf) : Http_parser.token =
    (!current_lexer) lexbuf
}
