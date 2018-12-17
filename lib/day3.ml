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
  include Comparator.Make(T)
end

module Claim = struct
  open Angstrom
  type t = {id: int; origin: Coordinate.t; size: int * int} [@@deriving sexp]

  let parser =
    let pint =
      take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string in
    let space = char ' ' in
    let tuple a b = (a, b) in
    let claimId = string "#" *> pint in
    let origin = lift2 tuple (pint <* string ",") (pint <* string ":") in
    let size = lift2 tuple (pint <* string "x") pint in
    lift3
      (fun claim co s -> {id= claim; origin= co; size= s})
      (claimId <* space *> string "@" *> space)
      (origin <* space) size

  let of_string s =
    parse_string parser s |> Result.ok_or_failwith

  let to_sexp c =
    sexp_of_t c
  
  (* a list of all coordinates taken up by the claim *)
  let claimed_coords c =
    [c.origin]
end

let%expect_test _ =
  Claim.of_string "#2 @ 1,3: 4x4"
  |> Claim.to_sexp |> Sexp.to_string |> Stdio.print_endline ;
  [%expect {|
  ((id 2)(coords(1 3))(size(4 4)))
  |}]

let recordOccurence map (claim: Claim.t) =
  match Map.find map claim.origin with
  | None ->
    let empty_set = Set.empty (module Int) in
    Map.set map ~key:claim.origin ~data: (Set.add empty_set claim.id)
  | Some claim_ids ->
    Map.set map ~key:claim.origin ~data:(Set.add claim_ids claim.id)

let claimed_squares claim_strings =
  let coords = Map.empty (module Coordinate) in
  let claims = List.map claim_strings ~f:Claim.of_string in
  List.fold claims ~init:coords ~f:recordOccurence

let multiple_claims claim_map =
  Map.filter claim_map ~f:(fun claims -> Set.length claims > 1)
  |> Map.keys
  |> List.length

let%expect_test _ =
  let claims = ["#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2";] in
  let multi_count = claimed_squares claims |> multiple_claims in
  printf "%i\n" multi_count;
  [%expect {|
  4
  |}]