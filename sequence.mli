open Std_internal

type 'a t

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val append : 'a t -> (unit -> 'a t) -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list

val is_empty : 'a t -> bool

val empty : 'a t

val iter : 'a t -> f:('a -> unit) -> unit

val read_sexps : in_channel -> Sexp.t t
