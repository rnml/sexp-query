open Std_internal
module Antiquote = struct
  type 'a t =
    | Unquote of 'a
    | Splice of 'a
  with sexp
end

module Template = struct
  type 'a t =
    | Hole of 'a
    | Atom of string
    | List of 'a t list

  let rec sexp_of_t sexp_of_a = function
    | Atom x -> Sexp.Atom x
    | List ts -> Sexp.List (List.map ts ~f:(fun t -> sexp_of_t sexp_of_a t))
    | Hole a -> sexp_of_a a

  let rec t_of_sexp a_of_sexp s =
    match Option.try_with (fun () -> a_of_sexp s) with
    | Some a -> Hole a
    | None ->
      match s with
      | Sexp.Atom x -> Atom x
      | Sexp.List xs -> List (List.map xs ~f:(fun x -> t_of_sexp a_of_sexp x))
end

module T = struct
  type t =
    | And of t * t
    | Atomic
    | Cat of t * t
    | Each
    | Fail
    | Field of string
    | If of t * t * t
    | Index of int
    | Not of t
    | Or of t * t
    | Pipe of t * t
    | Quote of t Antiquote.t Template.t
    | Smash
    | Test of t
    | This
    | Wrap of t

  let rec sexp_of_t = function
    | Atomic -> Sexp.Atom "atomic"
    | Each -> Sexp.Atom "each"
    | Fail -> Sexp.Atom "fail"
    | Field field -> Sexp.List [Sexp.Atom "field"; Sexp.Atom field]
    | This -> Sexp.Atom "this"
    | Smash -> Sexp.Atom "smash"
    | Not t -> Sexp.List [Sexp.Atom "if"; sexp_of_t t]
    | Test t -> Sexp.List [Sexp.Atom "test"; sexp_of_t t]
    | Wrap t -> Sexp.List [Sexp.Atom "wrap"; sexp_of_t t]
    | Index i -> Sexp.List [Sexp.Atom "index"; Int.sexp_of_t i]
    | Quote q ->
      Sexp.List [Sexp.Atom "quote"; Template.sexp_of_t (Antiquote.sexp_of_t sexp_of_t) q]
    | And (t1, t2) ->
      gather t1 t2 ~pat:(function And (a, b) -> Some (a, b) | _ -> None)
      |> fun ts -> Sexp.List (Sexp.Atom "and" :: ts)
    | Or (t1, t2) ->
      gather t1 t2 ~pat:(function Or (a, b) -> Some (a, b) | _ -> None)
      |> fun ts -> Sexp.List (Sexp.Atom "or" :: ts)
    | Pipe (t1, t2) ->
      gather t1 t2 ~pat:(function Pipe (a, b) -> Some (a, b) | _ -> None)
      |> fun ts -> Sexp.List (Sexp.Atom "pipe" :: ts)
    | Cat (t1, t2) ->
      gather t1 t2 ~pat:(function Cat (a, b) -> Some (a, b) | _ -> None)
      |> fun ts -> Sexp.List (Sexp.Atom "cat" :: ts)
    | If (t1, t2, t3) ->
      Sexp.List [Sexp.Atom "if"; sexp_of_t t1; sexp_of_t t2; sexp_of_t t3]

  and gather t1 t2 ~pat =
    let rec aux stack t acc =
      match pat t with
      | Some (a, b) -> aux (a :: stack) b acc
      | None ->
        let acc = sexp_of_t t :: acc in
        match stack with
        | [] -> acc
        | a :: stack -> aux stack a acc
    in
    aux [t1] t2 []

  let rec t_of_sexp = function
    | Sexp.Atom "atomic" -> Atomic
    | Sexp.Atom "each"   -> Each
    | Sexp.Atom "fail"   -> Fail
    | Sexp.Atom "this"   -> This
    | Sexp.Atom "smash"  -> Smash
    | Sexp.List [Sexp.Atom "not";   t] -> Not (t_of_sexp t)
    | Sexp.List [Sexp.Atom "field"; Sexp.Atom foo] -> Field foo
    | Sexp.List [Sexp.Atom "test"; t] -> Test (t_of_sexp t)
    | Sexp.List [Sexp.Atom "wrap"; t] -> Wrap (t_of_sexp t)
    | Sexp.List [Sexp.Atom "index"; i] -> Index (Int.t_of_sexp i)
    | Sexp.List [Sexp.Atom "if"; t1; t2; t3] ->
      If (t_of_sexp t1, t_of_sexp t2, t_of_sexp t3)
    | Sexp.List [Sexp.Atom "quote"; q] ->
      Quote (Template.t_of_sexp (Antiquote.t_of_sexp t_of_sexp) q)
    | Sexp.List (Sexp.Atom "and" :: args) ->
      List.fold_right args ~init:This ~f:(fun s acc -> And (t_of_sexp s, acc))
    | Sexp.List (Sexp.Atom "or" :: args) ->
      List.fold_right args ~init:Fail ~f:(fun s acc -> Or (t_of_sexp s, acc))
    | Sexp.List (Sexp.Atom "pipe" :: args) ->
      List.fold_right args ~init:This ~f:(fun s acc -> Pipe (t_of_sexp s, acc))
    | Sexp.List (Sexp.Atom "cat" :: args) ->
      List.fold_right args ~init:Fail ~f:(fun s acc -> Cat (t_of_sexp s, acc))
    | s -> failwiths "unrecognized query syntax" s (fun x -> x)

end
include T
include Sexpable.To_stringable (T)

let arg = Command.Spec.Arg_type.create of_string
