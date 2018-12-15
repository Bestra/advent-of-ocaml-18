open Core

let parse_entry e = 
  int_of_char e

let read_file () =
  In_channel.read_lines "./inputs/1.txt"
  |> List.map ~f:int_of_string

let part1 () =
  let lines = read_file () in

  List.fold ~init:0 ~f:(fun acc x -> acc + x) lines


type search_state = {
  seen: Base.Set.M(Int).t;
  previous: int
}

let rec reachedTwice (a_list: int sexp_list) count list_length (state: search_state) = 
  let idx = count % list_length in
  let currentFreq = List.nth_exn a_list idx in
  let newFreq = state.previous + currentFreq in
  match Base.Set.mem state.seen newFreq with
  | true ->
    newFreq
  | false -> 
    let newSeen = Base.Set.add state.seen newFreq in

    let nextState = {seen = newSeen; previous = newFreq} in
        reachedTwice a_list (count + 1) list_length nextState

let part2 () =
  let lines = read_file () in
  let new_set = Base.Set.empty (module Int) in
  reachedTwice lines 0 (List.length lines) {seen = new_set; previous = 0}

