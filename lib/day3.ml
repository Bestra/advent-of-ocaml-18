open Core
open Base

let read_file () = In_channel.read_lines "./inputs/3.txt"

module Coordinate = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]

    (* let compare (x1, x2) (y1, y2) =
       let cmp_first = Int.compare x1 x2 in
       if cmp_first <> 0 then cmp_first
       else Int.compare y1 y2 *)
  end

  include T
  include Comparator.Make (T)
end

module Claim = struct
  open Angstrom

  type t = {id: int; origin: Coordinate.t; size: int * int} [@@deriving sexp]

  let parser =
    let pint =
      take_while1 (function '0' .. '9' -> true | _ -> false)
      >>| Int.of_string
    in
    let space = char ' ' in
    let tuple a b = (a, b) in
    let claimId = string "#" *> pint in
    let origin = lift2 tuple (pint <* string ",") (pint <* string ":") in
    let size = lift2 tuple (pint <* string "x") pint in
    lift3
      (fun claim co s -> {id= claim; origin= co; size= s})
      (claimId <* space *> string "@" *> space)
      (origin <* space) size

  let of_string s = parse_string parser s |> Result.ok_or_failwith

  let to_sexp c = sexp_of_t c

  let nested_fold outer inner accum f =
    List.fold ~init:accum
      ~f:(fun a' o -> List.fold ~init:a' ~f:(fun a'' i -> f a'' o i) inner)
      outer

  let all_claimed_coords c =
    let xo, yo = c.origin in
    let w, h = c.size in
    let xs = List.range xo (xo + w) in
    let ys = List.range yo (yo + h) in
    nested_fold xs ys [] (fun accum x y -> (x, y) :: accum)

  (* a list of all coordinates taken up by the claim *)
end

let%expect_test _ =
  Claim.of_string "#2 @ 1,3: 4x4"
  |> Claim.to_sexp |> Sexp.to_string |> Stdio.print_endline ;
  [%expect {|
  ((id 2)(origin(1 3))(size(4 4)))
  |}]

let recordOccurence map (claim : Claim.t) =
  let loop m coords =
    match Map.find m coords with
    | None ->
        let empty_set = Set.empty (module Int) in
        Map.set m ~key:coords ~data:(Set.add empty_set claim.id)
    | Some claim_ids ->
        Map.set m ~key:coords ~data:(Set.add claim_ids claim.id)
  in
  List.fold (Claim.all_claimed_coords claim) ~init:map ~f:loop

let claimed_squares claim_strings =
  let coords = Map.empty (module Coordinate) in
  let claims = List.map claim_strings ~f:Claim.of_string in
  List.fold claims ~init:coords ~f:recordOccurence

let multiple_claims claim_map =
  Map.filter claim_map ~f:(fun claims -> Set.length claims > 1)
  |> Map.keys |> List.length

let duplicate_count s = s |> claimed_squares |> multiple_claims

let%expect_test _ =
  let claims = ["#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2"] in
  let multi_count = claimed_squares claims |> multiple_claims in
  printf "%i\n" multi_count ; [%expect {|
  4
  |}]

let part1 () = read_file () |> claimed_squares |> multiple_claims
