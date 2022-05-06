let call = Landmark.register "fib"
let main = Landmark.register "main"


let rec fib v =
  Landmark.wrap call 
    (fun (n, res) ->
    Minio.yield ();
    if n <= 1 then 
      res := !res + n 
    else 
      begin
        Minio.fork (fun () -> fib ((n-1),res));
        Minio.fork (fun () -> fib ((n-2),res))
      end)
       v

let () =
  let open Landmark in
  start_profiling
    ~profiling_options:{default_options with format = JSON; debug = true; allocated_bytes = true} ();
  enter main;
  Minio.run (fun () ->
    let res = ref 0 in
    fib (7, res);
    Printf.printf "%d\n%!" !res);
  exit main;
  if profiling () then begin
    let open Landmark.Graph in
    let cg = export () in
    let agg = aggregate_landmarks cg in
    let all_nodes = nodes agg in
    print_endline "\nLandmark reached:";
    all_nodes
    |> List.map (fun {name; _} -> name)
    |> List.sort compare
    |> List.iter print_endline
  end
