open Core
open Base

let read_file () = In_channel.read_lines "./inputs/2.txt"

let recordOccurence map item =
  match Map.find map item with
  | None -> Map.set map ~key:item ~data:1
  | Some count -> Map.set map ~key:item ~data:(count + 1)

let histogram list =
  let empty_map = Map.empty (module Int) in
  List.fold ~init:empty_map ~f:recordOccurence list

let hasRepeats (s : string) =
  let ma = String.to_list s |> List.map ~f:Char.to_int |> histogram in
  let hasCount k =
    match List.find ~f:(fun (_, v) -> v = k) (Map.to_alist ma) with
    | Some _ -> 1
    | None -> 0
  in
  (hasCount 2, hasCount 3)

let checksum lines =
  let t1, t2 =
    List.map lines ~f:hasRepeats
    |> List.fold
         ~f:(fun sum t ->
           let a, b = sum in
           let x, y = t in
           (a + x, b + y) )
         ~init:(0, 0)
  in
  t1 * t2

let part1 () = read_file () |> checksum
