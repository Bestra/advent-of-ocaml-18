open Angstrom
open Core
open Base

let read_file () = In_channel.read_lines "./inputs/4.txt"

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
  let to_string (t: t): string =
    Printf.sprintf "[%s %i] %s" (Date.to_string t.date) t.minutes (sexp_of_event t.event |> Sexp.to_string)
end

module Shift = struct
  type guard_state = Awake | Asleep [@@deriving sexp]

  type minute = int

  type t =
    { date: Date.t
    ; guard: int
    ; asleep: Interval.Int.t list
    ; minutes: guard_state array }
  [@@deriving sexp]

  (* let minutes_asleep t =
    List.fold t.asleep ~init:0 ~f:(fun total i ->
        let a, b = Interval.Int.bounds_exn i in
        let minutes = b - a + 1 in
        total + minutes ) *)

  let minutes_asleep t =
    Array.fold t.minutes ~init:0 ~f:(fun total i ->
        match i with Awake -> total | Asleep -> total + 1 )

  let to_chart_string t =
    let mins = Array.map t.minutes ~f:(function Awake -> 0 | Asleep -> 1) in
    Printf.sprintf "%s %i %i %s" (Date.to_string t.date) t.guard (minutes_asleep t) (sexp_of_array sexp_of_int mins|> Sexp.to_string)
end

exception Improper_entry of string

let shifts_of_entries (all_entries : ShiftEntry.t list) =
  let new_shift i e =
    { Shift.guard= i
    ; date= e.ShiftEntry.date
    ; asleep= []
    ; minutes= Array.create ~len:60 Shift.Awake }
  in
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
          go other_entries current_entry.minutes {current_shift with date= current_entry.date} shifts
      | WakesUp ->
          let sleep_interval =
            Interval.Int.create fell_asleep_at (current_entry.minutes - 1)
          in
          Array.fill current_shift.minutes ~pos:fell_asleep_at
            ~len:(current_entry.minutes - fell_asleep_at)
            Asleep ;
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

let real_shifts () =
  List.map (read_file ()) ~f:ShiftEntry.of_string
  |> List.sort ~compare:ShiftEntry.compare
  |> shifts_of_entries

let print_entries input =
  List.map input ~f:ShiftEntry.of_string
  |> List.sort ~compare:ShiftEntry.compare
  |> List.iter ~f:(fun i -> ShiftEntry.to_string i |> Stdio.print_endline)
 
let print_shifts input =
  List.map input ~f:ShiftEntry.of_string
  |> List.sort ~compare:ShiftEntry.compare
  |> shifts_of_entries
  |> List.iter ~f:(fun i -> Shift.to_chart_string i |> Stdio.print_endline)

let%expect_test _ =
  sample_shifts ()
  |> List.sexp_of_t Shift.sexp_of_t
  |> Sexp.to_string_hum |> Stdio.print_endline ;
  [%expect
    {|
(((date 1518-11-05) (guard 99) (asleep ((45 54)))
  (minutes
   (Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Asleep Asleep
    Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep Awake Awake Awake
    Awake Awake)))
 ((date 1518-11-04) (guard 99) (asleep ((36 45)))
  (minutes
   (Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake)))
 ((date 1518-11-03) (guard 10) (asleep ((24 28)))
  (minutes
   (Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Asleep Asleep Asleep Asleep Asleep Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake)))
 ((date 1518-11-01) (guard 99) (asleep ((40 49)))
  (minutes
   (Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake Awake Awake Asleep Asleep Asleep Asleep Asleep Asleep Asleep
    Asleep Asleep Asleep Awake Awake Awake Awake Awake Awake Awake Awake
    Awake Awake)))
 ((date 1518-11-01) (guard 10) (asleep ((30 54) (5 24)))
  (minutes
   (Awake Awake Awake Awake Awake Asleep Asleep Asleep Asleep Asleep Asleep
    Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep
    Asleep Asleep Asleep Asleep Awake Awake Awake Awake Awake Asleep Asleep
    Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep
    Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep Asleep
    Asleep Asleep Asleep Awake Awake Awake Awake Awake))))
|}]

let sorted_asleep_times (shifts : Shift.t list) =
  shifts
  |> List.map ~f:(fun e -> (e.guard, Shift.minutes_asleep e))
  |> Map.of_alist_reduce (module Int) ~f:(fun a b -> a + b)
  |> Map.to_alist
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> List.rev

type int_tuple = int * int [@@deriving sexp]

let sleepy_guard shifts =
  let sleepiest, _ = sorted_asleep_times shifts |> List.hd_exn in
  sleepiest

let%expect_test _ =
  sorted_asleep_times (sample_shifts ())
  |> List.sexp_of_t sexp_of_int_tuple
  |> Sexp.to_string_hum |> Stdio.print_endline ;
  [%expect {|
 ((10 50) (99 30))
|}]

let array_sum a b = Array.map2_exn a b ~f:( + )

let max_sleep_count_minute guard_id shifts =
  let minutes =
    List.filter_map
      ~f:(fun k ->
        if k.Shift.guard = guard_id then
          Some (Array.map k.minutes ~f:(function Awake -> 0 | Asleep -> 1))
        else None )
      shifts
  in
  let arr =
    List.reduce_exn ~f:array_sum minutes |> Array.mapi ~f:(fun i a -> (i, a))
  in
  Array.sort
    ~compare:(fun a b ->
      let _, x = a in
      let _, y = b in
      Int.compare y x )
    arr ;
  let (minute, _) = arr.(0) in
  (minute, guard_id)

let%expect_test _ =
  let shifts = sample_shifts () in
  let g = sleepy_guard shifts in
  max_sleep_count_minute g shifts
  |> sexp_of_int_tuple |> Sexp.to_string |> Stdio.print_endline ;
  [%expect {|
  (24 10)
  |}]

let part1 () =
  let shifts = real_shifts () in
  let g = sleepy_guard shifts in
  max_sleep_count_minute g shifts
  |> sexp_of_int_tuple |> Sexp.to_string

let part2 () = "foo"
