open Angstrom
open Core
open Base

let read_file () = In_channel.read_lines "./inputs/4.txt"

let part1 () = "foo"

let part2 () = "foo"

let sample_input =
  [ "[1518-11-01 00:00] Guard #10 begins shift"
  ; "[1518-11-01 00:05] falls asleep"
  ; "[1518-11-01 00:25] wakes up"
  ; "[1518-11-01 00:30] falls asleep"
  ; "[1518-11-01 00:55] wakes up"
  ; "[1518-11-01 23:58] Guard #99 begins shift"
  ; "[1518-11-02 00:40] falls asleep"
  ; "[1518-11-02 00:50] wakes up"
  ; "[1518-11-03 00:05] Guard #10 begins shift"
  ; "[1518-11-03 00:24] falls asleep"
  ; "[1518-11-03 00:29] wakes up"
  ; "[1518-11-04 00:02] Guard #99 begins shift"
  ; "[1518-11-04 00:36] falls asleep"
  ; "[1518-11-04 00:46] wakes up"
  ; "[1518-11-05 00:03] Guard #99 begins shift"
  ; "[1518-11-05 00:45] falls asleep"
  ; "[1518-11-05 00:55] wakes up" ]

module ShiftEntry = struct
  type event = Begin of int | WakesUp | FallsAsleep [@@deriving sexp]

  type t = {date: Date.t; minutes: int; event: event} [@@deriving sexp]

  let compare t1 t2 =
    match Date.compare t1.date t2.date with
    | 0 -> Int.compare t1.minutes t2.minutes
    | d -> d

  let parser =
    let bracketed p = char '[' *> p <* char ']' in
    let pint =
      take_while1 (function '0' .. '9' -> true | _ -> false)
      >>| Int.of_string
    in
    let date =
      lift3
        (fun year month day -> (year, month, day))
        (pint <* char '-')
        (pint <* char '-')
        pint
    in
    let time = pint *> char ':' *> pint in
    let date_time = lift2 (fun d t -> (d, t)) (date <* char ' ') time in
    let asleep = string "falls asleep" *> return FallsAsleep in
    let wakes = string "wakes up" *> return WakesUp in
    let begin_shift =
      string "Guard #" *> pint <* string " begins shift" >>| fun id -> Begin id
    in
    let event = asleep <|> wakes <|> begin_shift in
    let entry =
      lift2 (fun a b -> (a, b)) (bracketed date_time <* char ' ') event
      >>| fun e ->
      let ((year, month, day), minutes), event = e in
      { date= Date.create_exn ~y:year ~m:(Month.of_int_exn month) ~d:day
      ; minutes
      ; event }
    in
    entry

  let of_string s : t = parse_string parser s |> Result.ok_or_failwith
end

module Shift = struct
  type guard_state = Awake | Asleep [@@deriving sexp]

  type minute = int

  type t = {date: Date.t; guard: int; asleep: Interval.Int.t list}
  [@@deriving sexp]

  let minutes_asleep t =
    List.fold t.asleep ~init:0 ~f:(fun total i ->
        let a, b = Interval.Int.bounds_exn i in
        let minutes = b - a + 1 in
        total + minutes )
end

exception Improper_entry of string

let shifts_of_entries (all_entries : ShiftEntry.t list) =
  let new_shift i e = {Shift.guard= i; date= e.ShiftEntry.date; asleep= []} in
  let rec go entries fell_asleep_at current_shift shifts =
    match entries with
    | [] -> current_shift :: shifts
    | current_entry :: other_entries -> (
      match current_entry.ShiftEntry.event with
      | Begin i ->
          go other_entries 0
            (new_shift i current_entry)
            (current_shift :: shifts)
      | FallsAsleep ->
          go other_entries current_entry.minutes current_shift shifts
      | WakesUp ->
          let sleep_interval =
            Interval.Int.create fell_asleep_at (current_entry.minutes - 1)
          in
          go other_entries 0
            {current_shift with asleep= sleep_interval :: current_shift.asleep}
            shifts )
  in
  match all_entries with
  | [] -> []
  | hd :: tl -> (
    match hd.event with
    | Begin i -> go tl 0 (new_shift i hd) []
    | _ -> raise (Improper_entry "first entry must begin a shift") )

let sample_shifts () =
  List.map sample_input ~f:ShiftEntry.of_string
  |> List.sort ~compare:ShiftEntry.compare
  |> shifts_of_entries

let%expect_test _ =
  sample_shifts ()
  |> List.sexp_of_t Shift.sexp_of_t
  |> Sexp.to_string_hum |> Stdio.print_endline ;
  [%expect {|
(((date 1518-11-05) (guard 99) (asleep ((45 54))))
 ((date 1518-11-04) (guard 99) (asleep ((36 45))))
 ((date 1518-11-03) (guard 10) (asleep ((24 28))))
 ((date 1518-11-01) (guard 99) (asleep ((40 49))))
 ((date 1518-11-01) (guard 10) (asleep ((30 54) (5 24)))))
|}]

let foo () =
  List.map sample_input ~f:ShiftEntry.of_string
  |> List.sort ~compare:ShiftEntry.compare
  |> shifts_of_entries
  |> List.map ~f:(fun e -> (e.guard, Shift.minutes_asleep e))

let bar () =
  List.map sample_input ~f:ShiftEntry.of_string
  |> List.sort ~compare:ShiftEntry.compare
  |> shifts_of_entries
  |> List.map ~f:(fun e -> (e.guard, Shift.minutes_asleep e))
  |> Map.of_alist_reduce (module Int) ~f:(fun a b -> a + b)
  |> Map.to_alist
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)

type int_tuple = int * int [@@deriving sexp]

let%expect_test _ =
  bar ()
  |> List.sexp_of_t sexp_of_int_tuple
  |> Sexp.to_string_hum |> Stdio.print_endline ;
  [%expect {|
 "foo"
|}]