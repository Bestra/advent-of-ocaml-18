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

  module Degrees = struct
    type list_t = char * int [@@deriving sexp]

    type t = int Map.M(Char).t [@@deriving sexp]

    let compare (n1, d1) (n2, d2) =
      match Int.compare d1 d2 with 0 -> Char.compare n1 n2 | l -> l

    let of_dag dag =
      let in_degree dag n =
        match Map.find dag n with Some i -> List.length i.ins | None -> 0
      in
      Map.keys dag
      |> List.map ~f:(fun k -> (k, in_degree dag k))
      |> List.sort ~compare
      |> Map.of_alist_exn (module Char)

    let sorted_list t = Map.to_alist t |> List.sort ~compare

    let zero_in_nodes t =
      t |> sorted_list
      |> List.filter_map ~f:(fun (c, i) ->
             match i with 0 -> Some c | _ -> None )
  end

  let topo_sort (edges : t) =
    let decrement_children node degree_map =
      let kids = children edges node in
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
      let d = (Degrees.sexp_of_t degree_map) |> Sexp.to_string_hum ~indent:1 in
      let z = (List.sexp_of_t sexp_of_char zero_queue) |> Sexp.to_string_hum in
      printf "output: %s degrees: %s queue: %s \n" o d z; *)
      match zero_queue with
      | [] -> output
      | hd :: tl ->
          let new_map, new_zeros = decrement_children hd degree_map in
          go
            (List.append output [hd])
            new_map
            (List.append tl new_zeros |> List.sort ~compare:Char.compare)
    in
    let degrees = Degrees.of_dag edges in
    go [] degrees (Degrees.zero_in_nodes degrees)

  (* subtract one from all the degrees of hd's children.  if any of them are now zero append them to the queue *)

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

let read_file () = In_channel.read_lines "./inputs/7.txt"

let part1 () =
  read_file () |> Dag.of_input_list |> Dag.topo_sort |> String.of_char_list

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
  sample_input |> Dag.of_input_list |> Dag.Degrees.of_dag
  |> Dag.Degrees.sorted_list
  |> List.sexp_of_t Dag.Degrees.sexp_of_list_t
  |> Sexp.to_string_hum |> Stdio.print_endline ;
  [%expect {|
  ((C 0) (A 1) (B 1) (D 1) (F 1) (E 3))
|}]

let%expect_test _ =
  sample_input |> Dag.of_input_list |> Dag.topo_sort |> String.of_char_list
  |> Stdio.print_endline ;
  [%expect {|
   CABDFE
   |}]
