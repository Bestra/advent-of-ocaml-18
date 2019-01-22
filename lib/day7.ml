open Core
open Base

exception Improper_entry of string

module Dag = struct
  type node_value = {ins: char list; outs: char list} [@@deriving fields, sexp]

  let[@deriving sexp] empty_node = {ins= []; outs= []}

  type t = node_value Map.M(Char).t [@@deriving sexp]

  let edge_of_string s =
    let l = Str.split (Str.regexp " ") s in
    let char_at n = List.nth_exn l n |> Char.of_string in
    (char_at 1, char_at 7)

  let add_edge (dag : t) e : t =
    let parent, child = e in
    let m1 =
      Map.update dag parent ~f:(function
          | None -> {empty_node with outs= [child]}
          | Some n -> {n with outs= child :: n.outs} )
    in
    Map.update m1 child ~f:(function
        | None -> {empty_node with ins= [parent]}
        | Some n -> {n with ins= parent :: n.ins} )

  let children dag node =
    let n = Map.find_exn dag node in
    List.sort ~compare:Char.compare n.outs

  let of_input_list s =
    s |> List.map ~f:edge_of_string
    |> List.fold ~init:(Map.empty (module Char)) ~f:add_edge

  let dot_of_edges (t : t) accessor =
    let node_to_string i o =
      [Char.to_string i; " -> "; Char.to_string o; ";"] |> String.concat
    in
    let lines =
      Map.fold t ~init:[] ~f:(fun ~key ~data acc ->
          let edges =
            List.map (accessor data) ~f:(fun x -> node_to_string key x)
          in
          edges :: acc )
    in
    List.concat lines |> String.concat ~sep:"\n"

  let dot_of_children (t : t) = dot_of_edges t outs

  let dot_of_parents (t : t) = dot_of_edges t ins
end

module StatefulGraph = struct
  type list_t = char * int [@@deriving sexp]

  type in_degrees = int Map.M(Char).t [@@deriving sexp]

  type state = {edges: Dag.t; completed: char list; in_degrees: in_degrees}

  let compare (n1, d1) (n2, d2) =
    match Int.compare d1 d2 with 0 -> Char.compare n1 n2 | l -> l

  let of_dag (dag : Dag.t) =
    let in_degree n =
      match Map.find dag n with Some i -> List.length i.ins | None -> 0
    in
    let in_degrees =
      Map.keys dag
      |> List.map ~f:(fun k -> (k, in_degree k))
      |> List.sort ~compare
      |> Map.of_alist_exn (module Char)
    in
    {edges= dag; completed= []; in_degrees}

  let zero_degree_nodes in_deg =
    in_deg |> Map.to_alist |> List.sort ~compare
    |> List.filter_map ~f:(fun (c, i) -> match i with 0 -> Some c | _ -> None)

  let complete state node =
    let kids = Dag.children state.edges node in
    (* let k = (List.sexp_of_t sexp_of_char kids) |> Sexp.to_string_hum in
        printf "Children of %c are %s\n" node k; *)
    List.fold kids ~init:state ~f:(fun {in_degrees; completed; _} kid ->
        let old_val = Map.find_exn in_degrees kid in
        let new_val = old_val - 1 in
        let new_completed =
          match new_val with
          | 0 -> List.append completed [kid]
          | _ -> completed
        in
        { state with
          in_degrees= Map.set in_degrees ~key:kid ~data:new_val
        ; completed= new_completed } )

  let to_complete state =
    let char_set = Set.of_list (module Char) state.completed in
    List.filter ~f:(fun x -> Set.mem char_set x |> not) (zero_degree_nodes state.in_degrees)

  let topo_sort t =
    let decrement_children node degree_map =
      let kids = Dag.children t.edges node in
      (* let k = (List.sexp_of_t sexp_of_char kids) |> Sexp.to_string_hum in
         printf "Children of %c are %s\n" node k; *)
      List.fold kids ~init:(degree_map, []) ~f:(fun (m, zeros) kid ->
          let old_val = Map.find_exn m kid in
          let new_val = old_val - 1 in
          let new_zeros =
            match new_val with 0 -> List.append zeros [kid] | _ -> zeros
          in
          (Map.set m ~key:kid ~data:new_val, new_zeros) )
    in
    let rec go output degree_map zero_queue =
      (* let o = (List.sexp_of_t sexp_of_char output) |> Sexp.to_string_hum in
         let d = (StatefulGraph.sexp_of_t degree_map) |> Sexp.to_string_hum ~indent:1 in
         let z = (List.sexp_of_t sexp_of_char zero_queue) |> Sexp.to_string_hum in
         printf "output: %s StatefulGraph: %s queue: %s \n" o d z; *)
      match zero_queue with
      | [] -> output
      | hd :: tl ->
        let new_map, new_zeros = decrement_children hd degree_map in
        go
          (List.append output [hd])
          new_map
          (List.append tl new_zeros |> List.sort ~compare:Char.compare)
    in
    go [] t.in_degrees (zero_degree_nodes t.in_degrees)
end

module Scheduler = struct
  type worker = Working of {node: char; done_at: int} | Free
  [@@deriving sexp]

  let run_graph worker_count a_time sorted_nodes =
    let finish_jobs current_time workers out_nodes =
      List.fold workers ~init:([], out_nodes) ~f:(fun (workers, nodes) w ->
          match w with
          | Free -> (w :: workers, nodes)
          | Working {node; done_at} ->
            if current_time >= done_at then (Free :: workers, node :: nodes)
            else (w :: workers, nodes) )
    in
    let start_jobs current_time workers in_nodes =
      let duration n = Char.to_int n - 65 + a_time in
      List.fold workers ~init:([], in_nodes) ~f:(fun (workers, nodes) w ->
          match w with
          | Working _ -> (w :: workers, nodes)
          | Free -> (
              match nodes with
              | [] -> (w :: workers, nodes)
              | hd :: tl ->
                ( Working {node= hd; done_at= duration hd + current_time}
                  :: workers
                , tl ) ) )
    in
    let rec advance second workers input_nodes finished_nodes =
      let p_char_list l = List.sexp_of_t sexp_of_char l |> Sexp.to_string in
      printf "%i %s %s %s \n" second
        (List.sexp_of_t sexp_of_worker workers |> Sexp.to_string)
        (p_char_list input_nodes)
        (p_char_list finished_nodes) ;
      match input_nodes with
      | [] -> (second, finished_nodes)
      | nodes ->
        (* first finish existing jobs, push those nodes to out  *)
        let updated_workers, newly_finished =
          finish_jobs second workers finished_nodes
        in
        (* then pull off nodes from in to give to free workers*)
        let final_workers, consumed_inputs =
          start_jobs second updated_workers nodes
        in
        advance (second + 1) final_workers consumed_inputs newly_finished
    in
    advance 0
      (List.range 0 worker_count |> List.map ~f:(fun _ -> Free))
      sorted_nodes []
end

let read_file () = In_channel.read_lines "./inputs/7.txt"

let part1 () =
  read_file () |> Dag.of_input_list |> StatefulGraph.of_dag
  |> StatefulGraph.topo_sort |> String.of_char_list

let part2 () = "bar"

let sample_input =
  [ "Step C must be finished before step A can begin."
  ; "Step C must be finished before step F can begin."
  ; "Step A must be finished before step B can begin."
  ; "Step A must be finished before step D can begin."
  ; "Step B must be finished before step E can begin."
  ; "Step D must be finished before step E can begin."
  ; "Step F must be finished before step E can begin." ]

let%expect_test _ =
  sample_input |> Dag.of_input_list |> Dag.dot_of_children
  |> Stdio.print_endline ;
  [%expect
    {|
  F -> E;
  D -> E;
  C -> F;
  C -> A;
  B -> E;
  A -> D;
  A -> B;
|}]

let%expect_test _ =
  sample_input |> Dag.of_input_list |> Dag.dot_of_parents
  |> Stdio.print_endline ;
  [%expect
    {|
  F -> C;
  E -> F;
  E -> D;
  E -> B;
  D -> A;
  B -> A;
  A -> C;
|}]

let%expect_test _ =
  sample_input |> Dag.of_input_list |> StatefulGraph.of_dag
  |> StatefulGraph.topo_sort |> String.of_char_list |> Stdio.print_endline ;
  [%expect {|
   CABDFE
   |}]

let%expect_test _ =
  ['C'; 'A'; 'B'; 'D'; 'F'; 'E'] |> Scheduler.run_graph 2 1 |> ignore ;
  [%expect {|
   CABDFE
   |}]
