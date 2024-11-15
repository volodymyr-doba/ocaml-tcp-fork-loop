open Lwt
open Lwt.Syntax
open Random

let listen_address = Unix.inet_addr_loopback
let backlog = 10

let random_wait min max () =
   let promise, resolver = Lwt.wait () in
   Lwt.async (fun () ->
      (*   Не спрацював random in range     *)
      (*   Тому:    *)
      (*   1. Генеруємо випадковий float від 0 до (max - min) *)
      (*   2. Додаємо до нього min  *)
      (*    *)
      let seconds = min +. (Random.float (max -. min)) in
      Lwt_unix.sleep seconds >|=
      Lwt.wakeup resolver);
   promise

let rec handle_connection ic oc () =
    Lwt_io.read_line_opt ic >>=
    (fun msg ->
        match msg with
        | Some msg ->
            Logs_lwt.info (fun m -> m "Server: received '%s'" msg)
            >>= fun () -> handle_connection ic oc ()
        | None -> Logs_lwt.info (fun m -> m "Server: connection closed") >>= Lwt.return)

let accept_connection ch conn =
    let rec listen (oc) =
        Lwt_condition.wait ch
            >>= fun () -> Lwt_io.write_line oc "THERE IS SHOULD BE A MESSAGE"
            >>= fun () -> listen oc in

    let fd, _ = conn in
        let ic = Lwt_io.of_fd Lwt_io.Input fd in
        let oc = Lwt_io.of_fd Lwt_io.Output fd in

        listen oc;

        Lwt.on_failure (handle_connection ic oc ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
        Logs_lwt.info (fun m -> m "Server: new connection") >>= Lwt.return

let create_socket () =
    let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
        Lwt_unix.bind sock @@ Unix.ADDR_INET(listen_address, 54321);
        Lwt_unix.listen sock backlog;
        Logs_lwt.info (fun m -> m "Server: listening on %s:%d" (Unix.string_of_inet_addr listen_address) 54321);
    sock

let create_server ch sock =
    let rec serve () =
        Lwt_unix.accept sock >>= (accept_connection ch) >>= serve
    in serve


let rec keep_alive (oc) =
    random_wait 5. 20. ()
        >>= fun () -> Lwt_io.write_line oc Printf.(sprintf "Child (%d): i am alive" (Unix.getpid ()))
        >>= fun () -> keep_alive oc

let child_listen () =
    Lwt_io.with_connection (Unix.ADDR_INET(listen_address, 54321)) (fun (ic, oc) ->
        keep_alive oc;
        let rec loop () =
            Lwt_io.read_line_opt ic >>= function
            | Some msg ->
                 Logs_lwt.info (fun m -> m "Child (%d): received '%s'" (Unix.getpid ()) msg)
                 >>= fun () -> Lwt_io.write_line oc Printf.(sprintf "%d" (String.length msg))
                 >>= loop
            | None -> Logs_lwt.info (fun m -> m "Child (%d): connection closed" (Unix.getpid ())) >>= Lwt.return
        in Lwt.pick [loop();]
    )

let rec infinite_loop () =
      Lwt_unix.sleep 1.
    >>= infinite_loop

let suicide () =
    random_wait 60. 120. ()
        >>= fun() -> Logs_lwt.info (fun m -> m "Child (%d): suicide time" (Unix.getpid ()))

let serve_stdin (ch) =
    let rec serve () =
            Lwt_io.read_line_opt Lwt_io.stdin
                >>= fun(line) ->
                  match line with
                      | Some line ->
                          Logs_lwt.info (fun m -> m "Parent: stdin '%s'" line);
                          Lwt_condition.broadcast ch ();
                          Lwt.return();
         >>= serve
    in serve

let () =
    let () = Logs.set_reporter (Logs.format_reporter ()) in
    let () = Logs.set_level (Some Logs.Info) in

    let sock = create_socket () in
    let ch = Lwt_condition.create ()  in
    let serve = create_server ch sock in

    let stdin = serve_stdin ch in
    let active = ref 0 in
    let expected = int_of_string Sys.argv.(1) in

    Lwt.async (fun () -> serve());

    Lwt_main.run @@ (
        let rec loop () =
          match expected > !active with
            | false ->
                Lwt.pick [
                        stdin();
                        Lwt_unix.wait ()
                            >>= fun(i) ->
                                active := !active - 1;
                                Logs_lwt.info (fun m -> m "Parent: child is dead")
                    ] >>= loop
            | true ->
              let pid = Lwt_unix.fork () in
                match pid with
                    | 0 ->
                        Random.self_init();
                        Lwt.async (fun () -> child_listen());
                        Lwt.pick [suicide();]
                            >>= fun(i) -> exit 0
                    | pid ->
                        active := !active + 1;
                        Logs_lwt.info (fun m -> m "Parent: child pid %d (active %d, expected %d)" pid !active expected);
                        loop()
        in loop();
    )
