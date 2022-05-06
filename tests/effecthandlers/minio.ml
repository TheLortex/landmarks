
open EffectHandlers
open EffectHandlers.Deep

type _ eff += Fork  : (unit -> unit) -> unit eff
type _ eff += Yield : unit eff
type _ eff += Sleep : float -> unit eff

let fork f =
  perform (Fork f)

let yield () =
  perform Yield

let sleep timeout =
  perform (Sleep timeout)

type timeout =
  | Sleep of (unit, unit) continuation

type runnable =
  | Thread : ('a, unit) continuation * 'a -> runnable

type state =
  { run_q : runnable Queue.t;
    sleep_ht : (float, timeout) Hashtbl.t; }

let init () =
  { run_q = Queue.create ();
    sleep_ht = Hashtbl.create 13; }

let enqueue_thread st k x =
  Queue.push (Thread(k, x)) st.run_q

let dequeue st =
  match Queue.pop st.run_q with
  | Thread(k, x) -> continue k x

let block_sleep st span k =
  let time = Unix.gettimeofday () +. span in
  Hashtbl.add st.sleep_ht time (Sleep k)

(* Wakes up sleeping threads.
*
* Returns [(b, t)] where [t] is the eariest time in the future when a thread
* needs to wake up, and [b] is true if some thread is woken up.
*)
let wakeup st now : bool * float =
  let (l,w,n) =
    Hashtbl.fold
      (fun t (Sleep k) (l, w, next) ->
        if t <= now then
          (enqueue_thread st k (); (t::l, true, next))
        else if t < next then
          (l, w, t)
        else (l, w, next))
      st.sleep_ht ([], false, max_float)
  in
  List.iter (fun t -> Hashtbl.remove st.sleep_ht t) l;
  (w, n)

let rec schedule st =
  if Queue.is_empty st.run_q then (* No runnable threads *)
    if Hashtbl.length st.sleep_ht = 0 then () (* We are done. *)
    else
      let now = Unix.gettimeofday () in
      let (thrd_has_woken_up, next_wakeup_time) = wakeup st now in
      if thrd_has_woken_up then
        schedule st
      else if next_wakeup_time = max_float then
        assert false
      else (
      Unix.sleepf (next_wakeup_time -. now);
      ignore (wakeup st (Unix.gettimeofday ()));
      schedule st
      )
  else (* Still have runnable threads *)
    dequeue st

let run main =
  let st = init () in
  let rec fork st f =
    match_with f () {
      retc = (fun () -> schedule st);
      exnc = (fun exn ->
        print_string (Printexc.to_string exn);
        schedule st);
      effc = fun (type a) (e : a eff) ->
        match e with
        | Yield -> Some (fun (k : (a, _) continuation) ->
            enqueue_thread st k ();
            schedule st)
        | Fork f -> Some (fun k ->
            enqueue_thread st k ();
            fork st f)
        | Sleep t -> Some (fun k ->
            if t <= 0. then continue k ()
            else begin
              block_sleep st t k;
              schedule st
            end)
        | _ -> None
    }
  in
  fork st main
