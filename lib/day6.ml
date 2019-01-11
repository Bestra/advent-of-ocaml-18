open Core
open Base

let read_file () = In_channel.read_lines "./inputs/4.txt"

let sample_input =
  ["1, 1" ; "1, 6" ; "8, 3" ; "3, 4" ; "5, 5" ; "8, 9"]

type input_pt = {
  x: int;
  y: int;
  label: char
}
exception Improper_entry of string

let point_of_string i s =
  let coords = Str.split (Str.regexp ", ") s
  |> List.map ~f:Int.of_string in
  match coords with
  | x :: y  :: _ -> {x=x; y=y; label= (Char.of_int_exn (i + 67))}
  | _ -> raise (Improper_entry "Foo")

let list_to_points l =
  List.mapi ~f:point_of_string l

let bounding_box inputs =
  let xs = List.map ~f:(fun k -> k.x) inputs in
  let ys = List.map ~f:(fun k -> k.y) inputs in
  let get_min = List.min_elt ~compare:Int.compare in
  let get_max = List.max_elt ~compare:Int.compare in
  ((get_min xs, get_min ys), (get_max xs, get_max ys))

let nested_fold outer inner accum f =
  List.fold ~init:accum
    ~f:(fun a' o -> List.fold ~init:a' ~f:(fun a'' i -> f a'' o i) inner)
    outer

let nearest x y inputs =
  

let make_grid bounds =
let ((x1, y1), (x2, y2)) = bounds in
let xs = List.range x1 (x2 + 1) in
let ys = List.range y1 (y2 + 1) in

(xs, ys)