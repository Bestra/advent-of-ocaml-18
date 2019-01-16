open Core
open Base

exception Improper_entry of string

module Dag = struct
  type node_value = {ins: char list; outs: char list} [@@deriving fields]

  let empty_node = {ins= []; outs= []}

  type t = node_value Map.M(Char).t

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

  type in_degree = (char * int) [@@deriving sexp]

  let in_degrees dag =
    let in_degree dag n =
      match Map.find dag n with Some i -> List.length i.ins | None -> 0
    in

    Map.keys dag
    |> List.map ~f:(fun k -> (k, in_degree dag k))
    |> List.sort ~compare:(fun (n1, d1) (n2, d2) ->
      match Int.compare d1 d2 with
      | 0 -> Char.compare n1 n2
      | l -> l
    )

  let topo_sort (degrees: in_degree list)

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

let read_file () = In_channel.read_lines "./inputs/6.txt"

let part1 () = "foo"

let part2 () = "bar"

let sample_input =
  [ "Step C must be finished before step A can begin."
  ; "Step C must be finished before step F can begin."
  ; "Step A must be finished before step B can begin."
  ; "Step A must be finished before step D can begin."
  ; "Step B must be finished before step E can begin."
  ; "Step K must be finished before step A can begin."
  ; "Step D must be finished before step E can begin."
  ; "Step F must be finished before step E can begin." ]

let%expect_test _ =
  sample_input |> Dag.of_input_list |> Dag.dot_of_children
  |> Stdio.print_endline ;
  [%expect
    {|
  K -> A;
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
  A -> K;
  A -> C;
|}]

let%expect_test _ =
  sample_input |> Dag.of_input_list |> Dag.in_degrees
  |> List.sexp_of_t Dag.sexp_of_in_degree
  |> Sexp.to_string_hum
  |> Stdio.print_endline ;
  [%expect
    {|
  ((C 0) (K 0) (B 1) (D 1) (F 1) (A 2) (E 3))
|}]

(* let%expect_test _ =
   sample_input |> Dag.of_input_list |> Dag.to_dot |> Stdio.print_endline;
   [%expect {|
   CKABDFE
   |}] *)
