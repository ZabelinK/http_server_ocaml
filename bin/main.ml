open Core
open Lwt.Infix

type config = {directory: string; port: int}

let parse_http_request message =
  let lexbuf = Lexing.from_string message in
  try
    let _result = Http_parser.main Http_lexer.read_token lexbuf in
    Logs.debug (fun m -> m "Method: %s" _result.method_) ;
    Logs.debug (fun m -> m "Path: %s" _result.path) ;
    Logs.debug (fun m -> m "Version: %s" _result.version) ;
    List.iter _result.headers ~f:(fun (key, value) ->
        Logs.debug (fun m -> m "Header: %s %s" key value) ) ;
    Ok ()
  with
  | Stdlib.Parsing.Parse_error ->
      let pos = Lexing.lexeme_start lexbuf in
      let token = Lexing.lexeme lexbuf in
      Error
        (Printf.sprintf "Parsing Error at position %d: unexpected token '%s'"
           pos token )
  | Failure msg -> Error ("Failed : " ^ msg)

let string_of_sockaddr (addr : Caml_unix.sockaddr) : string =
  match addr with
  | Caml_unix.ADDR_INET (inet_addr, port) ->
      Printf.sprintf "%s:%d" (Caml_unix.string_of_inet_addr inet_addr) port
  | Caml_unix.ADDR_UNIX path -> Printf.sprintf "Unix socket: %s" path

let handle_client _ (addr : Caml_unix.sockaddr)
    ((ic : Lwt_io.input_channel), (_ : Lwt_io.output_channel)) =
  Logs.info (fun m -> m "New client from - %s" (string_of_sockaddr addr)) ;
  Lwt_io.read ic
  >>= fun message ->
  match parse_http_request message with
  | Ok () ->
      Logs_lwt.info (fun m -> m "Done Parsing") >>= fun () -> Lwt.return_unit
  | Error err ->
      Logs.err (fun m -> m "Error while parsing: %s" err) ;
      Lwt.return_unit

let start (c : config) =
  Logs.debug (fun m -> m "Directory to work with : %s" c.directory) ;
  let handle_client_with_config = handle_client c in
  Lwt_io.establish_server_with_client_address
    (Lwt_unix.ADDR_INET (Caml_unix.inet_addr_any, c.port))
    handle_client_with_config
  >>= fun _server -> fst (Lwt.wait ())

let start_http_server (c : config) = Lwt_main.run (start c)

let command =
  Command.basic ~summary:"Simple HTTP OCaml server"
    (let%map_open.Command directory = anon ("directory" %: string)
     and port = anon (maybe ("port" %: int)) in
     fun () ->
       let config = {directory; port= Option.value port ~default:8080} in
       start_http_server config )

let () =
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  Logs.set_level (Some Logs.Debug) ;
  Command_unix.run ~version:"0.1" ~build_info:"DEBUG" command
