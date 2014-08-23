open Std_internal

module Antiquote : sig
  type 'a t =
    | Unquote of 'a
    | Splice of 'a
  with sexp
end

module Template : sig
  type 'a t =
    | Hole of 'a
    | Atom of string
    | List of 'a t list
  with sexp
end

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
with sexp

include Stringable with type t := t

val arg : t Command.Spec.Arg_type.t
