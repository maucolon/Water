(*
  dispenser.ml
  ------------------------------------------------------------
  Final OCaml controller for Arduino dual-pump transfer system

  Based on original beta client structure:
    - REPL mode
    - one-command mode
    - script mode
    - TCP direct communication with Arduino

  Hardware model:
    - Pump1 transfers A -> B
    - Pump2 transfers B -> A

  Supported user commands:
    hello
    status
    measure
    stop
    dispense <liters>           (* legacy alias for A -> B / Pump1 *)
    transfer A B <liters>
    transfer B A <liters>
    pump1 <liters>              (* explicit Pump1 alias *)
    pump2 <liters>              (* explicit Pump2 alias *)
    help
    quit / exit

  Device protocol sent to Arduino:
    hello
    status
    measure
    stop
    dispense,<liters>
    transfer,A,B,<liters>
    transfer,B,A,<liters>

  Run:
    dune exec ./dispenser.exe -- --host 192.168.1.142 --port 4080
*)

open Unix

(* ========================================================= *)
(* TYPES                                                     *)
(* ========================================================= *)

type tank =
  | A
  | B

type pump_channel =
  | Pump1   (* A -> B *)
  | Pump2   (* B -> A *)

type cmd =
  | Hello
  | Status
  | Stop
  | Measure
  | Dispense of float
  | Transfer of tank * tank * float
  | RunPump of pump_channel * float
  | Help
  | Quit
  | Empty
  | Unknown of string

(* ========================================================= *)
(* STRING HELPERS                                            *)
(* ========================================================= *)

let trim (s : string) : string =
  let is_space = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  in
  let n = String.length s in
  let i = ref 0 in
  while !i < n && is_space s.[!i] do
    incr i
  done;
  let j = ref (n - 1) in
  while !j >= !i && is_space s.[!j] do
    decr j
  done;
  if !j < !i then ""
  else String.sub s !i (!j - !i + 1)

let split_ws (s : string) : string list =
  s
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")

let tank_of_string (s : string) : (tank, string) result =
  match String.uppercase_ascii (trim s) with
  | "A" -> Ok A
  | "B" -> Ok B
  | other -> Error ("Unknown tank: " ^ other)

let string_of_tank = function
  | A -> "A"
  | B -> "B"

let pump_channel_to_transfer = function
  | Pump1 -> (A, B)
  | Pump2 -> (B, A)

(* ========================================================= *)
(* PARSER                                                    *)
(* ========================================================= *)

let parse_positive_float (liters : string) : (float, string) result =
  try
    let f = float_of_string liters in
    if f <= 0.0 then Error "Liters must be > 0"
    else Ok f
  with _ ->
    Error "Bad liters value"

let parse_cmd (line : string) : cmd =
  let t = String.lowercase_ascii (trim line) in
  match split_ws t with
  | [] ->
      Empty

  | ["hello"] ->
      Hello

  | ["status"] ->
      Status

  | ["stop"] ->
      Stop

  | ["measure"] ->
      Measure

  | ["help"] ->
      Help

  | ["quit"] | ["exit"] ->
      Quit

  | ["dispense"; liters] -> (
      match parse_positive_float liters with
      | Ok f -> Dispense f
      | Error msg -> Unknown msg
    )

  | ["pump1"; liters] -> (
      match parse_positive_float liters with
      | Ok f -> RunPump (Pump1, f)
      | Error msg -> Unknown msg
    )

  | ["pump2"; liters] -> (
      match parse_positive_float liters with
      | Ok f -> RunPump (Pump2, f)
      | Error msg -> Unknown msg
    )

  | ["transfer"; src; dst; liters] -> (
      match tank_of_string src, tank_of_string dst, parse_positive_float liters with
      | Ok src_tank, Ok dst_tank, Ok f ->
          if src_tank = dst_tank then
            Unknown "Source and destination must be different"
          else
            Transfer (src_tank, dst_tank, f)
      | Error msg, _, _ -> Unknown msg
      | _, Error msg, _ -> Unknown msg
      | _, _, Error msg -> Unknown msg
    )

  | _ ->
      Unknown
        "Unknown command (try: hello | status | measure | stop | dispense 2 | transfer A B 2 | pump1 2 | pump2 2)"

(* ========================================================= *)
(* COMMAND -> DEVICE LINE                                    *)
(* ========================================================= *)

let cmd_to_device_line (c : cmd) : (string, string) result =
  match c with
  | Hello ->
      Ok "hello"
  | Status ->
      Ok "status"
  | Measure ->
      Ok "measure"
  | Stop ->
      Ok "stop"
  | Dispense f ->
      Ok (Printf.sprintf "dispense,%.3f" f)
  | Transfer (src, dst, f) ->
      Ok (Printf.sprintf "transfer,%s,%s,%.3f" (string_of_tank src) (string_of_tank dst) f)
  | RunPump (channel, f) ->
      let src, dst = pump_channel_to_transfer channel in
      Ok (Printf.sprintf "transfer,%s,%s,%.3f" (string_of_tank src) (string_of_tank dst) f)
  | Help ->
      Error "help"
  | Quit ->
      Error "quit"
  | Empty ->
      Error "Empty command"
  | Unknown msg ->
      Error msg

(* ========================================================= *)
(* SOCKET READ                                               *)
(* ========================================================= *)

let read_line_from_sock (sock : Unix.file_descr) ~(max_bytes : int) : string =
  let buf1 = Bytes.create 1 in
  let b = Buffer.create 128 in
  let rec loop count =
    if count >= max_bytes then
      Buffer.contents b
    else
      match Unix.read sock buf1 0 1 with
      | 0 ->
          Buffer.contents b
      | _ ->
          let c = Bytes.get buf1 0 in
          if c = '\n' then
            Buffer.contents b
          else (
            Buffer.add_char b c;
            loop (count + 1)
          )
  in
  loop 0

(* ========================================================= *)
(* SEND LINE                                                 *)
(* ========================================================= *)

let send_line ~(host : string) ~(port : int) ~(timeout_s : float) (line : string) : string =
  let addr = (gethostbyname host).h_addr_list.(0) in
  let sockaddr = ADDR_INET (addr, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  try
    Unix.setsockopt_float sock SO_RCVTIMEO timeout_s;
    Unix.setsockopt_float sock SO_SNDTIMEO timeout_s;

    connect sock sockaddr;

    let msg = line ^ "\n" in
    ignore (Unix.write_substring sock msg 0 (String.length msg));

    let resp = read_line_from_sock sock ~max_bytes:2048 |> trim in
    close sock;
    resp
  with e ->
    (try close sock with _ -> ());
    raise e

(* ========================================================= *)
(* HELP                                                      *)
(* ========================================================= *)

let print_help () =
  print_endline "Commands:";
  print_endline "  hello";
  print_endline "  status";
  print_endline "  measure";
  print_endline "  stop";
  print_endline "  dispense <liters>        # legacy alias for Pump1 / A -> B";
  print_endline "  transfer A B <liters>    # tank-based transfer";
  print_endline "  transfer B A <liters>    # tank-based transfer";
  print_endline "  pump1 <liters>           # explicit Pump1 / A -> B";
  print_endline "  pump2 <liters>           # explicit Pump2 / B -> A";
  print_endline "  help";
  print_endline "  quit";
  print_endline ""

(* ========================================================= *)
(* RUN ONE                                                   *)
(* ========================================================= *)

let run_one ~(host : string) ~(port : int) ~(timeout_s : float) (user_line : string) : unit =
  match parse_cmd user_line with
  | Help ->
      print_help ()
  | Quit ->
      print_endline "bye"
  | parsed -> (
      match cmd_to_device_line parsed with
      | Error "help" ->
          print_help ()
      | Error "quit" ->
          print_endline "bye"
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
    )

(* ========================================================= *)
(* REPL                                                      *)
(* ========================================================= *)

let run_repl ~(host : string) ~(port : int) ~(timeout_s : float) : unit =
  Printf.printf "OCaml Dispenser REPL -> %s:%d\n" host port;
  print_help ();
  let rec loop () =
    print_string "> ";
    Stdlib.flush Stdlib.stdout;
    match read_line () with
    | exception End_of_file ->
        ()
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

(* ========================================================= *)
(* FILE MODE                                                 *)
(* ========================================================= *)

let run_file ~(host : string) ~(port : int) ~(timeout_s : float) (path : string) : unit =
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

(* ========================================================= *)
(* MAIN                                                      *)
(* ========================================================= *)

let () =
  let host = ref "10.202.37.224" in
  let port = ref 4080 in
  let timeout_s = ref 3.0 in
  let cmd_arg : string option ref = ref None in
  let file_arg : string option ref = ref None in

  let usage =
    "dispenser [--host IP] [--port N] [--timeout SEC] [--cmd \"...\"] [--file path]\n"
    ^ "Modes:\n"
    ^ "  REPL:   no --cmd/--file\n"
    ^ "  One:    --cmd \"transfer A B 2\"\n"
    ^ "  One:    --cmd \"pump1 2\"\n"
    ^ "  Script: --file myscript.ds\n"
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
  | (Some c, _) ->
      run_one ~host:!host ~port:!port ~timeout_s:!timeout_s c
  | (None, Some f) ->
      run_file ~host:!host ~port:!port ~timeout_s:!timeout_s f
  | (None, None) ->
      run_repl ~host:!host ~port:!port ~timeout_s:!timeout_s