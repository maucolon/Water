(*This program talks to Arduino directly and used as beta test

User commands implemented:
hello
status
dispense <x>
stop
measure

The Ino code has been modified to accept these 4 words

Modes:
REPL: No arguments
-One line: --cmd "dispense 2"
-Script: -file myscript.ds (one command per line; in a file you can configure various commands; however they are limited to one per line)
TO Run:
dune exec ./dispenser.exe -- --host 192.168.1.142 --port 4080
*)
open Unix
type cmd = 
    | Hello
    | Status 
    | Stop 
    | Dispense of float 
    | Empty 
    | Measure
    | Unknown of string

let trim (s : string) : string =
    let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in 
    let n = String.length s in 
    let i = ref 0 in 
    while !i < n && is_space s.[!i] do 
      incr i 
    done;
    let j = ref (n-1) in 
    while !j >= !i && is_space s.[!j] do 
      decr j 
    done;
    if !j < !i then "" 
    else String.sub s !i (!j - !i + 1)

let split_ws (s : string) : string list =
  s
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")

let parse_cmd (line : string) : cmd =
  let t = String.lowercase_ascii (trim line) in
  match split_ws t with  
  | [] -> Empty 
  | ["hello"] -> Hello
  | ["status"] -> Status
  | ["stop"] -> Stop
  | ["measure"] -> Measure
  | ["dispense"; liters] -> (
        try 
          let f = float_of_string liters in 
          if f <= 0.0 then Unknown "Liters must be > 0"
          else Dispense f
        with _ -> Unknown "Bad liters. Example: dispense 0.5"
        )
  | _ -> Unknown "Unknown command (try: hello | status | measure | stop | dispense 2)"

let cmd_to_device_line (c : cmd) : (string, string) result = 
  match c with 
  | Hello -> Ok "hello"
  | Status -> Ok "status"
  | Measure -> Ok "measure"
  | Stop -> Ok "stop"
  | Dispense f -> Ok (Printf.sprintf "dispense,%.3f" f)
  | Empty -> Error "Empty command"
  | Unknown msg -> Error msg

let read_line_from_sock (sock : Unix.file_descr) ~(max_bytes:int) : string =
  let buf1 = Bytes.create 1 in
  let b = Buffer.create 128 in
  let rec loop count =
    if count >= max_bytes then Buffer.contents b
    else
      match Unix.read sock buf1 0 1 with
      | 0 -> Buffer.contents b (* EOF *)
      | _ ->
          let c = Bytes.get buf1 0 in
          if c = '\n' then Buffer.contents b
          else (Buffer.add_char b c; loop (count + 1))
  in
  loop 0

let send_line ~(host:string) ~(port:int) ~(timeout_s:float) (line:string) : string =
  let addr = (gethostbyname host).h_addr_list.(0) in
  let sockaddr = ADDR_INET (addr, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  try
    (* timeouts *)
    Unix.setsockopt_float sock SO_RCVTIMEO timeout_s;
    Unix.setsockopt_float sock SO_SNDTIMEO timeout_s;

    connect sock sockaddr;

    let msg = line ^ "\n" in
    ignore (Unix.write_substring sock msg 0 (String.length msg));

    (* read one newline-terminated line (max 2048 bytes) *)
    let resp = read_line_from_sock sock ~max_bytes:2048 |> trim in
    close sock;
    resp
  with e ->
    (try close sock with _ -> ());
    raise e

let print_help () = 
    print_endline "Commands:";
    print_endline " hello";
    print_endline " status";
    print_endline " stop";
    print_endline "  dispense <int_liters>";
    print_endline "  help";
    print_endline "  quit";
    print_endline "  measure";
    print_endline ""

let run_one ~(host:string) ~(port:int) ~(timeout_s:float) (user_line:string) : unit =
  match cmd_to_device_line (parse_cmd user_line) with
  | Error msg ->
      Printf.printf "error: %s\n" msg
  | Ok device_line ->
      Printf.printf "sent: %s\n" device_line;
      (try
         let resp = send_line ~host ~port ~timeout_s device_line in
         Printf.printf "resp: %s\n" (if resp = "" then "(no response)" else resp)
       with
       | Unix_error (e, _, _) ->
           Printf.printf "net error: %s\n" (Unix.error_message e)
       | e ->
           Printf.printf "error: %s\n" (Printexc.to_string e))

let run_repl ~(host:string) ~(port:int) ~(timeout_s:float) : unit =
  Printf.printf "OCaml Dispenser REPL -> %s:%d\n" host port;
  print_help ();
  let rec loop () =
    print_string "> ";
    Stdlib.flush Stdlib.stdout;
    match read_line () with
    | exception End_of_file -> ()
    | line ->
        let t = String.lowercase_ascii (trim line) in
        if t = "quit" || t = "exit" then
          print_endline "bye"
        else if t = "help" then
          (print_help (); loop ())
        else
          (run_one ~host ~port ~timeout_s line; loop ())
  in
  loop ()

let run_file ~(host:string) ~(port:int) ~(timeout_s:float) (path:string) : unit =
  let ic = open_in path in
  let rec loop line_no =
    match input_line ic with
    | line ->
        let t = trim line in
        if t = "" || (String.length t >= 1 && t.[0] = '#') then
          loop (line_no + 1)
        else (
          Printf.printf "\n[%d] %s\n" line_no t;
          run_one ~host ~port ~timeout_s t;
          loop (line_no + 1)
        )
    | exception End_of_file ->
        close_in ic
  in
  loop 1


let () =
  let host = ref "10.202.37.224" in
  let port = ref 4080 in
  let timeout_s = ref 3.0 in
  let cmd_arg : string option ref = ref None in
  let file_arg : string option ref = ref None in

  let usage =
    "dispenser [--host IP] [--port N] [--timeout SEC] [--cmd \"...\"] [--file path]\n" ^
    "Modes:\n" ^
    "  REPL:   no --cmd/--file\n" ^
    "  One:    --cmd \"dispense 2\"\n" ^
    "  Script: --file myscript.ds\n"
  in

  let speclist =
    [
      ("--host", Arg.Set_string host, "Arduino IP address");
      ("--port", Arg.Set_int port, "Arduino TCP port");
      ("--timeout", Arg.Set_float timeout_s, "Socket timeout seconds");
      ("--cmd", Arg.String (fun s -> cmd_arg := Some s), "Run a single command then exit");
      ("--file", Arg.String (fun s -> file_arg := Some s), "Run commands from file (one per line)");
    ]
  in

  let anon_fun s =
    prerr_endline ("Unexpected argument: " ^ s);
    prerr_endline usage;
    exit 2
  in

  Arg.parse speclist anon_fun usage;

  match (!cmd_arg, !file_arg) with
  | (Some c, _) -> run_one ~host:!host ~port:!port ~timeout_s:!timeout_s c
  | (None, Some f) -> run_file ~host:!host ~port:!port ~timeout_s:!timeout_s f
  | (None, None) -> run_repl ~host:!host ~port:!port ~timeout_s:!timeout_s