
(** We write our PID here when we're ready to receive connections. *)
let default_pidfile = "/var/run/fe.pid"

open Fe_debug

let setup sock cmdargs id_to_fd_map syslog_stdout env =
  let fd_sock_path = Printf.sprintf "/var/xapi/forker/fd_%s" 
    (Uuidm.to_string (Uuidm.create `V4)) in
  let fd_sock = Fecomms.open_unix_domain_sock () in
  Fecomms.unlink_safe fd_sock_path;
  debug "About to bind to %s" fd_sock_path;
  Unix.bind fd_sock (Unix.ADDR_UNIX fd_sock_path);
  Unix.listen fd_sock 5;
  debug "bound, listening";
  let result = Unix.fork () in
  if result=0 
  then begin
    debug "Child here!";
    let result2 = Unix.fork () in
    if result2=0 then begin
      debug "Grandchild here!";
      (* Grandchild *)
      let state = {
	Child.cmdargs=cmdargs; 
	env=env;
	id_to_fd_map=id_to_fd_map; 
	syslog_stdout={Child.enabled=syslog_stdout.Fe.enabled; Child.key=syslog_stdout.Fe.key};
	ids_received=[];
	fd_sock2=None;
	finished=false;
      } in
      Child.run state sock fd_sock fd_sock_path
    end else begin
      (* Child *)
      exit 0;
    end
  end else begin
    (* Parent *)
    debug "Waiting for process %d to exit" result;
    ignore(Unix.waitpid [] result);
    Unix.close fd_sock;
    Some {Fe.fd_sock_path=fd_sock_path}
  end

let do_daemonize () =
        match Unix.fork () with
        | 0 ->
                if Unix.setsid () == -1 then
                        failwith "Unix.setsid failed";

                begin match Unix.fork () with
                | 0 ->
                        let nullfd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
                        begin try
                                Unix.close Unix.stdin;
                                Unix.dup2 nullfd Unix.stdout;
                                Unix.dup2 nullfd Unix.stderr;
                        with exn -> Unix.close nullfd; raise exn
                        end;
                        Unix.close nullfd
                | _ -> exit 0
                end
        | _ -> exit 0

(** write a pidfile file *)
let pidfile_write filename =
        let fd = Unix.openfile filename
                               [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ]
                               0o640 in
        Fecomms.finally
        (fun () ->
                let pid = Unix.getpid () in
                let buf = string_of_int pid ^ "\n" in
                let len = String.length buf in
                if Unix.write fd buf 0 len <> len 
                then failwith "pidfile_write failed";
        )
        (fun () -> Unix.close fd)


let _ =
  let pidfile = ref default_pidfile in
  let daemonize = ref false in
 
  Arg.parse (Arg.align [
	       "-daemon", Arg.Set daemonize, "Create a daemon";
	       "-pidfile", Arg.Set_string pidfile, Printf.sprintf "Set the pid file (default \"%s\")" !pidfile;
	     ])
    (fun _ -> failwith "Invalid argument")
    "Usage: fe [-daemon] [-pidfile filename]";

  if !daemonize then do_daemonize ();

  Sys.set_signal Sys.sigpipe (Sys.Signal_ignore);

  let main_sock = Fecomms.open_unix_domain_sock_server "/var/xapi/forker/main" in

  pidfile_write !pidfile;

  (* At this point the init.d script should return and we are listening on our socket. *)

  while true do
    try
      let (sock,addr) = Unix.accept main_sock in
      reset ();
      let cmd = Fecomms.read_raw_rpc sock in
      match cmd with
	| Fe.Setup s ->
	    let result = setup sock s.Fe.cmdargs s.Fe.id_to_fd_map s.Fe.syslog_stdout s.Fe.env in
	    (match result with
	      | Some response ->
		  Fecomms.write_raw_rpc sock (Fe.Setup_response response);
		  Unix.close sock;
	      | _ -> ())
	| _ -> 
	    debug "Ignoring invalid message";
	    Unix.close sock
    with e -> 
      debug "Caught exception at top level: %s" (Printexc.to_string e);
  done
      
    
      
