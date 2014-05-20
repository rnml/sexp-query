open Std_internal

open Syntax

let fail = Sequence.empty
let yield = Sequence.return

let rec smash s =
  Sequence.append (yield s) (fun () ->
    match s with
    | Sexp.Atom _ -> fail
    | Sexp.List ss -> Sequence.bind (Sequence.of_list ss) smash
  )

let each = function
  | Sexp.Atom _ -> fail
  | Sexp.List ss -> Sequence.of_list ss

let field foo = function
  | Sexp.Atom _ -> fail
  | Sexp.List ss -> Sequence.bind (Sequence.of_list ss) (function
    | Sexp.List [Sexp.Atom bar; value] when String.equal foo bar ->
      Sequence.return value
    | _ ->
      Sequence.empty
  )

let atomic = function
  | Sexp.Atom _ as s -> yield s
  | Sexp.List _ -> fail

type quote_result =
  | Col of Sexp.t Sequence.t (* column to iterate through *)
  | Row of Sexp.t list (* row to splice in *)
  | One of Sexp.t (* a singleton row/column *)

let rec run_one t s =
  match t with
  | Smash         -> smash s
  | This          -> yield s
  | Pipe (t1, t2) -> Sequence.bind (run_one t1 s) (fun s -> run_one t2 s)
  | Fail          -> fail
  | Cat (t1, t2)  -> Sequence.append (run_one t1 s) (fun () -> run_one t2 s)
  | Each          -> each s
  | Atomic        -> atomic s
  | Wrap t        -> yield (Sexp.List (Sequence.to_list (run_one t s)))
  | Field f       -> field f s
  | Test t ->
    let result = run_one t s in
    if Sequence.is_empty result
    then fail
    else yield s
  | Not t ->
    let result = run_one t s in
    if Sequence.is_empty result
    then yield s
    else fail
  | Or (t1, t2) ->
    let result = run_one t1 s in
    if not (Sequence.is_empty result)
    then result
    else run_one t2 s
  | And (t1, t2) ->
    let result = run_one t1 s in
    if not (Sequence.is_empty result)
    then run_one t2 s
    else fail
  | If (t1, t2, t3) ->
    let result = run_one t1 s in
    if not (Sequence.is_empty result)
    then run_one t2 s
    else run_one t3 s
  | Quote q ->
    match quote q s with
    | Col ss -> ss
    | Row ss -> Sequence.of_list ss
    | One s -> Sequence.return s

and quote q s =
  match q with
  | Template.Atom _ -> One s
  | Template.Hole (Antiquote.Unquote t) -> Col (run_one t s)
  | Template.Hole (Antiquote.Splice t) -> Row (Sequence.to_list (run_one t s))
  | Template.List qs ->
    let rec rows
      : Syntax.t Antiquote.t Template.t list -> Sexp.t list Sequence.t
      = function
        | [] -> Sequence.return []
        | q :: qs ->
          let rows = rows qs in
          match quote q s with
          | Col col ->
            Sequence.bind col (fun s ->
              Sequence.map rows (fun ss -> s :: ss))
          | Row row ->
            Sequence.map rows (fun ss -> row @ ss)
          | One x ->
            Sequence.map rows (fun ss -> x :: ss)
    in
    Col (Sequence.map (rows qs) ~f:(fun ss -> Sexp.List ss))

let run t ss = Sequence.bind ss (fun s -> run_one t s)
