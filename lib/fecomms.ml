open Fe

let finally f g =
  try
    let x = f () in
    g ();
    x
  with e ->
    g ();
    raise e

let open_unix_domain_sock () =
  Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0

(** create a directory but doesn't raise an exception if the directory already exist *)
let mkdir_safe dir perm =
	try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(** create a directory, and create parent if doesn't exist *)
let mkdir_rec dir perm =
	let rec p_mkdir dir =
		let p_name = Filename.dirname dir in
		if p_name <> "/" && p_name <> "." 
		then p_mkdir p_name;
		mkdir_safe dir perm in
	p_mkdir dir

(** remove a file, but doesn't raise an exception if the file is already removed *)
let unlink_safe file =
	try Unix.unlink file with (* Unix.Unix_error (Unix.ENOENT, _ , _)*) _ -> ()

let open_unix_domain_sock_server path =
  mkdir_rec (Filename.dirname path) 0o755;
  unlink_safe path;
  let sock = open_unix_domain_sock () in
  try
    Unix.bind sock (Unix.ADDR_UNIX path);
    Unix.listen sock 5;
    sock
  with e ->
    Unix.close sock;
    raise e

let open_unix_domain_sock_client path =
  let sock = open_unix_domain_sock () in
  try 
    Unix.connect sock (Unix.ADDR_UNIX path);
    sock
  with e ->
    Unix.close sock;
    raise e

let rec really_read fd string off n =
  if n=0 then () else
    let m = Unix.read fd string off n in
    if m = 0 then raise End_of_file;
    really_read fd string (off+m) (n-m)

let really_read_string fd length =
  let buf = String.make length '\000' in
  really_read fd buf 0 length;
  buf

let read_raw_rpc sock =
  let buffer = String.make 12 '\000' in
  really_read sock buffer 0 12;
  let len = int_of_string buffer in
  let body = really_read_string sock len in
  ferpc_of_rpc (Jsonrpc.of_string body)


let really_write fd string off n =
	let written = ref 0 in
	while !written < n
	do
		let wr = Unix.write fd string (off + !written) (n - !written) in
		written := wr + !written
	done

(* Ideally, really_write would be implemented with optional arguments ?(off=0) ?(len=String.length string) *)
let really_write_string fd string =
	really_write fd string 0 (String.length string)

let write_raw_rpc sock ferpc =
  let body = Jsonrpc.to_string (rpc_of_ferpc ferpc) in
  let len = String.length body in
  let buffer = Printf.sprintf "%012d%s" len body in
  really_write_string sock buffer

exception Connection_closed

let receive_named_fd sock =
  let buffer = String.make 36 '\000' in
  let (len,from,newfd) = Fd_send_recv.recv_fd sock buffer 0 36 [] in  
  if len=0 then raise Connection_closed;
  (newfd,buffer)

let send_named_fd sock uuid fd =
  ignore(Fd_send_recv.send_fd sock uuid 0 (String.length uuid) [] fd)
  
    
