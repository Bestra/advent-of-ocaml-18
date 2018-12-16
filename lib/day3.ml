open Core
open Angstrom

let read_file () = In_channel.read_lines "./inputs/3.txt"

type claim = {id: int; coords: int * int; size: int * int} [@@deriving sexp]

let pint =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let space = char ' '

let parseClaim =
  let tuple a b = (a, b) in
  let claimId = string "#" *> pint in
  let coords = lift2 tuple (pint <* string ",") (pint <* string ":") in
  let size = lift2 tuple (pint <* string "x") pint in
  lift3
    (fun claim co s -> {id= claim; coords= co; size= s})
    (claimId <* space *> string "@" *> space)
    (coords <* space) size

let%expect_test _ =
  parse_string parseClaim "#2 @ 1,3: 4x4"
  |> Result.ok_or_failwith |> sexp_of_claim |> Sexp.to_string |> print_endline ;
  [%expect {|
  ((id 2)(coords(1 3))(size(4 4)))
  |}]
