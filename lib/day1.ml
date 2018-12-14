open Core

let parse_entry e = 
  int_of_char e

let read_file () =
  In_channel.read_lines "./inputs/1.txt"

let part1 () =
  let lines = read_file () in

  printf "running over %i lines" (List.length lines);
  List.map ~f:int_of_string lines
  |> List.fold ~init:0 ~f:(fun acc x -> acc + x)


type search_state = {
  seen: Base.Set.M(Int).t;
  previous: int
}

let rec reachedTwice array count array_length (state: search_state) = 
  let idx = count % array_length in
  let currentFreq = array.[idx] in
  let newFreq = state.previous + currentFreq in
  match Base.Set.mem state.seen newFreq with
  | true ->
    newFreq
  | false -> 
    Base.Set.add state.seen newFreq

let nextState = {seen = state.seen; previous = newFreq}
    reachedTwice array (count + 1) arrayLength nextState

let part2 () =
  printf "Finding part 2"

let file = read_file ()
    reachedTwice file 0 (Array.length file) {seen = new HashSet<int>(); previous = 0} |> sprintf "%i"

