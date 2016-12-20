(*
#load "unix.cma";;
#load "request.cmo";;

let port = 12346;;
*)
let port = int_of_string Sys.argv.(1)

let () =
  Unix.establish_server (fun ic oc ->
    try
      let lexbuf = Lexing.from_channel ic in
      while true do
        output_string oc
          begin match Request.token lexbuf with
          | Request.Quit -> raise End_of_file
          | Request.Noop -> "OK\r\n"
          | Request.Move board ->
              begin match Player.play board with
              | None -> "XX\r\n"
              | Some move -> Printf.sprintf "%s\r\n" (Move.to_string move)
              end
          end;
        flush oc
      done
    with End_of_file | Failure _ -> close_in ic)
    (Unix.ADDR_INET (Unix.inet_addr_any, port))
;;
