open Core
open Base

exception Improper_entry of string

let nested_fold outer inner accum f =
  List.fold ~init:accum
    ~f:(fun a' o -> List.fold ~init:a' ~f:(fun a'' i -> f a'' o i) inner)
    outer

type distance = char * int [@@deriving sexp]

type char_int_map = int Map.M(Char).t [@@deriving sexp]

module Grid = struct
  type point_location = Interior | Exterior [@@deriving sexp]

  type nearest = Input of distance | Tie [@@deriving sexp]

  type point = {r: int; c: int; nearest: nearest; location: point_location}
  [@@deriving sexp]

  type input_pt = {r: int; c: int; label: char} [@@deriving sexp]

  type t =
    {inputs: input_pt list; bounds: int * int * int * int; points: point list}
  [@@deriving sexp]

  let point_of_string i s =
    let coords = Str.split (Str.regexp ", ") s |> List.map ~f:Int.of_string in
    match coords with
    | c :: r :: _ -> {r; c; label= Char.of_int_exn (i + 97)}
    | _ -> raise (Improper_entry "Foo")

  let list_to_points l = List.mapi ~f:point_of_string l

  let get_distance (i : input_pt) r c =
    let dr = abs (i.r - r) in
    let dc = abs (i.c - c) in
    let out = dr + dc in
    out

  let input_distances c r inputs =
    List.map inputs ~f:(fun j -> (j.label, get_distance j c r))
    |> List.sort ~compare:(fun (_, d1) (_, d2) -> Int.compare d1 d2)

  let nearest_input c r inputs =
    match input_distances c r inputs with
    | (i1, d1) :: (_, d2) :: _ -> if d1 = d2 then Tie else Input (i1, d1)
    | hd :: _ -> Input hd
    | [] -> failwith "Empty list of inputs"

  let bounding_box inputs =
    let rs = List.map ~f:(fun k -> k.r) inputs in
    let cs = List.map ~f:(fun k -> k.c) inputs in
    let get_min x = List.min_elt x ~compare:Int.compare |> Option.value_exn in
    let get_max x = List.max_elt x ~compare:Int.compare |> Option.value_exn in
    let expand (a, b, c, d) = (a - 1, b - 1, c + 1, d + 1) in
    expand (get_min rs, get_min cs, get_max rs, get_max cs)

  let make str =
    let inputs = list_to_points str in
    let r1, c1, r2, c2 = bounding_box inputs in
    let rs = List.range r1 (r2 + 1) in
    let cs = List.range c1 (c2 + 1) in
    let pts =
      nested_fold rs cs [] (fun a r c ->
          let location =
            match (r, c) with
            | a, b when a = r1 || a = r2 || b = c1 || b = c2 -> Exterior
            | _ -> Interior
          in
          let nearest = nearest_input r c inputs in
          {r; c; nearest; location} :: a )
    in
    {inputs; bounds= (r1, c1, r2, c2); points= List.rev pts}

  let interior_groups g =
    let group_type points =
      match List.find points ~f:(fun p -> phys_equal p.location Exterior) with
      | Some _ -> Exterior
      | None -> Interior
    in
    List.filter_map g.points ~f:(fun point ->
        match point.nearest with
        | Tie -> None
        | Input (label, _) -> Some (label, point) )
    |> Map.of_alist_fold (module Char) ~init:[] ~f:(fun acc p -> p :: acc)
    |> Map.filter ~f:(fun v ->
           match group_type v with Interior -> true | Exterior -> false )
    |> Map.map ~f:(fun v -> List.length v)

  let safe_region_size max_distance g =
    List.filter_map g.points ~f:(fun p ->
        let total =
          input_distances p.c p.r g.inputs
          |> List.fold ~init:0 ~f:(fun sum (_label, dist) -> sum + dist)
        in
        if total < max_distance then Some total else None )
    |> List.length

  let to_string g =
    let point_strings =
      List.map g.points ~f:(fun x ->
          match x.nearest with
          | Input (c, 0) ->
              Printf.sprintf "%c" (Char.of_int_exn (Char.to_int c - 32))
          | Input (c, _) -> Printf.sprintf "%c" c
          | Tie -> Printf.sprintf "." )
    in
    let _, c1, _, c2 = g.bounds in
    let width = c2 - c1 + 1 in
    let groups =
      List.groupi ~break:(fun i _ _ -> i % width = 0) point_strings
    in
    List.map groups ~f:(fun x -> String.concat x) |> String.concat ~sep:"\n"
end

let read_file () = In_channel.read_lines "./inputs/6.txt"

let part1 () =
  read_file () |> Grid.make |> Grid.interior_groups |> sexp_of_char_int_map
  |> Sexp.to_string_hum

let part2 () =
  read_file () |> Grid.make
  |> Grid.safe_region_size 10000
  |> Int.sexp_of_t |> Sexp.to_string

let%expect_test _ =
  let sample_input = ["1, 1"; "1, 6"; "8, 3"; "3, 4"; "5, 5"; "8, 9"] in
  (* let sample_input = ["1, 1"; "3, 3"] in *)
  Grid.make sample_input |> Grid.to_string |> Stdio.print_endline ;
  [%expect
    {|
aaaaa.cccc
aAaaa.cccc
aaaddecccc
aadddeccCc
..dDdeeccc
bb.deEeecc
bBb.eeee..
bbb.eeefff
bbb.eeffff
bbb.ffffFf
bbb.ffffff
|}]

let%expect_test _ =
  let sample_input = ["1, 1"; "1, 6"; "8, 3"; "3, 4"; "5, 5"; "8, 9"] in
  (* let sample_input = ["1, 1"; "3, 3"] in *)
  Grid.make sample_input |> Grid.interior_groups |> sexp_of_char_int_map
  |> Sexp.to_string_hum |> Stdio.print_endline ;
  [%expect {|
    ((d 9) (e 17))
|}]

let%expect_test _ =
  let sample_input = ["1, 1"; "1, 6"; "8, 3"; "3, 4"; "5, 5"; "8, 9"] in
  (* let sample_input = ["1, 1"; "3, 3"] in *)
  Grid.make sample_input |> Grid.safe_region_size 32 |> Int.sexp_of_t
  |> Sexp.to_string |> Stdio.print_endline ;
  [%expect {|
    16
|}]
