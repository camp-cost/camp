type payload =
  | Integer of int
  | String of string
  | Unit

let a_to_s : payload Queue.t = Queue.create ()
let s_to_a : payload Queue.t = Queue.create ()

let a_to_s_mutex = Mutex.create ()
let s_to_a_mutex = Mutex.create ()

let b_to_s : payload Queue.t = Queue.create ()
let s_to_b : payload Queue.t = Queue.create ()

let b_to_s_mutex = Mutex.create ()
let s_to_b_mutex = Mutex.create ()

let a_to_b : payload Queue.t = Queue.create ()
let b_to_a : payload Queue.t = Queue.create ()

let a_to_b_mutex = Mutex.create ()
let b_to_a_mutex = Mutex.create ()

let with_mutex mutex f arg =
  Mutex.lock mutex;
  let result = f arg in
  Mutex.unlock mutex;
  result

let put queue mutex item =
  with_mutex mutex (Queue.add item) queue

let rec get queue mutex =
  Mutex.lock mutex;
  if Queue.is_empty queue
  then begin
    Mutex.unlock mutex;
    Unix.sleep 1; (* Sleep 1 sec *)
    get queue mutex
    end
  else begin
    let result = Queue.take queue in
    Mutex.unlock mutex;
    result
    end

let put_int x (queue, mutex)
  = put queue mutex (Integer x)

let put_string s (queue, mutex)
  = put queue mutex (String s)

let put_unit () (queue, mutex)
  = put queue mutex Unit

let get_int (queue, mutex)
  = match get queue mutex with
    | Integer i -> i
    | _ -> failwith "Type mismatch"

let get_string (queue, mutex)
  = match get queue mutex with
    | String s -> s
    | _ -> failwith "Type mismatch"

let get_unit (queue, mutex)
  = match get queue mutex with
    | Unit -> ()
    | _ -> failwith "Type mismatch"

let qs_for_A = function
  | GeneratedTwoBuyerA.B -> a_to_b, a_to_b_mutex
  | GeneratedTwoBuyerA.S -> a_to_s, a_to_s_mutex

let qr_for_A = function
  | GeneratedTwoBuyerA.B -> b_to_a, b_to_a_mutex
  | GeneratedTwoBuyerA.S -> s_to_a, s_to_a_mutex

let commA : GeneratedTwoBuyerA.communications = {
  send_int = (fun r v -> put_int v (qs_for_A r));
  send_string = (fun r v -> put_string v (qs_for_A r));
  send_unit = (fun r v -> put_unit v (qs_for_A r));
  recv_int = (fun r _ -> get_int (qr_for_A r));
  recv_string = (fun r _ -> get_string (qr_for_A r));
  recv_unit = (fun r _ -> get_unit (qr_for_A r));
}

let runA () =
  GeneratedTwoBuyerA.run Impl_A.handlers commA

let qs_for_B = function
  | GeneratedTwoBuyerB.A -> b_to_a, b_to_a_mutex
  | GeneratedTwoBuyerB.S -> b_to_s, b_to_s_mutex

let qr_for_B = function
  | GeneratedTwoBuyerB.A -> a_to_b, a_to_b_mutex
  | GeneratedTwoBuyerB.S -> s_to_b, s_to_b_mutex

let commB : GeneratedTwoBuyerB.communications = {
  send_int = (fun r v -> put_int v (qs_for_B r));
  send_string = (fun r v -> put_string v (qs_for_B r));
  send_unit = (fun r v -> put_unit v (qs_for_B r));
  recv_int = (fun r _ -> get_int (qr_for_B r));
  recv_string = (fun r _ -> get_string (qr_for_B r));
  recv_unit = (fun r _ -> get_unit (qr_for_B r));
}

let runB () =
  GeneratedTwoBuyerB.run Impl_B.handlers commB

let qs_for_S = function
  | GeneratedTwoBuyerS.A -> s_to_a, s_to_a_mutex
  | GeneratedTwoBuyerS.B -> s_to_b, s_to_b_mutex

let qr_for_S = function
  | GeneratedTwoBuyerS.A -> a_to_s, a_to_s_mutex
  | GeneratedTwoBuyerS.B -> b_to_s, b_to_s_mutex

let commS : GeneratedTwoBuyerS.communications = {
  send_int = (fun r v -> put_int v (qs_for_S r));
  send_string = (fun r v -> put_string v (qs_for_S r));
  send_unit = (fun r v -> put_unit v (qs_for_S r));
  recv_int = (fun r _ -> get_int (qr_for_S r));
  recv_string = (fun r _ -> get_string (qr_for_S r));
  recv_unit = (fun r _ -> get_unit (qr_for_S r));
}

let runS () =
  GeneratedTwoBuyerS.run Impl_S.handlers commS

let () =
  let t = Unix.gettimeofday () in
  Random.init ((Unix.time ()) |> int_of_float);
  (* Printf.printf "Initialising\n%!"; *)
  let a_thread = Thread.create runA () in
  (* Printf.printf "Started Buyer A\n%!"; *)
  let b_thread = Thread.create runB () in
  (* Printf.printf "Started Buyer B\n%!"; *)
  let s_thread = Thread.create runS () in
  (* Printf.printf "Started Server\n%!"; *)
  Thread.join a_thread;
  Thread.join b_thread;
  Thread.join s_thread;
  Printf.printf "Execution time: %f seconds\n"
                (Unix.gettimeofday () -. t)
