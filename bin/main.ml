open Core
open Lwt.Infix

type config = { directory : string; port : int }

type http_response = {
  version : string;
  return_code : int;
  return_message : string;
  headers : (string * string) list;
  data : string;
}

let parse_http_request message =
  let lexbuf = Lexing.from_string message in
  Http_parser.main Http_lexer.read_token lexbuf

(*with | Stdlib.Parsing.Parse_error -> let pos = Lexing.lexeme_start lexbuf in
  let token = Lexing.lexeme lexbuf in Error (Printf.sprintf "Parsing Error at
  position %d: unexpected token '%s'" pos token ) | Failure msg -> Error
  ("Failed : " ^ msg)*)

let string_of_sockaddr (addr : Caml_unix.sockaddr) : string =
  match addr with
  | Caml_unix.ADDR_INET (inet_addr, port) ->
      Printf.sprintf "%s:%d" (Caml_unix.string_of_inet_addr inet_addr) port
  | Caml_unix.ADDR_UNIX path -> Printf.sprintf "Unix socket: %s" path

let handle_http_request (config : config) (request : Ast.http_request_header) =
  match request.method_ with
  | "GET" ->
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Input (config.directory ^ request.path)
            (fun ic -> Lwt_io.read ic)
          >>= fun page ->
          Lwt.return
            {
              version = request.version;
              return_code = 200;
              return_message = "OK";
              headers = [ ("Content-Type", "text/html") ];
              data = page;
            })
        (fun _ ->
          Lwt.return
            {
              version = request.version;
              return_code = 404;
              return_message = "Not Found";
              headers =
                [ ("Content-Type", "text/html"); ("Connection", "keep-alive") ];
              data = "<html><body>Not Found</body></html>";
            })
  | _ ->
      Lwt.return
        {
          version = request.version;
          return_code = 405;
          return_message = "Method Not Allowed";
          headers = [ ("Allow", "GET"); ("Content-type", "text/plain") ];
          data = "";
        }
(* prepare response in async manner*)

let http_response_to_string http_response =
  let status_line =
    Printf.sprintf "%s %d %s\r\n" http_response.version
      http_response.return_code http_response.return_message
  in
  let headers_lines =
    List.map http_response.headers ~f:(fun (key, value) ->
        Printf.sprintf "%s: %s\r\n" key value)
    |> String.concat
  in
  Printf.sprintf "%s%s\r\n%s" status_line headers_lines http_response.data

let handle_client config (addr : Caml_unix.sockaddr)
    ((ic : Lwt_io.input_channel), (oc : Lwt_io.output_channel)) =
  Logs.info (fun m -> m "New client from - %s" (string_of_sockaddr addr));
  Lwt.catch
    (fun () ->
      Lwt_io.read ic >>= fun message ->
      print_endline message;
      let http_request_header = parse_http_request message in
      Logs_lwt.info (fun m -> m "Got request %s" http_request_header.method_)
      >>= fun () ->
      handle_http_request config http_request_header >>= fun response ->
      Lwt_io.write oc (http_response_to_string response) >>= fun () ->
      Logs_lwt.info (fun m ->
          m "Sent response \n%s" (http_response_to_string response)))
    (fun exp ->
      Logs_lwt.err (fun m -> m "Failed to read message %s" (Exn.to_string exp)))

let start (c : config) =
  Logs.debug (fun m -> m "Directory to work with : %s" c.directory);
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
       let config = { directory; port = Option.value port ~default:8080 } in
       start_http_server config)

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Command_unix.run ~version:"0.1" ~build_info:"DEBUG" command
