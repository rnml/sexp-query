open Std_internal

type 'a seq =
  | Done
  | Cons of 'a * 'a tail

and 'a t =
  | Empty
  | Append of 'a t * (unit -> 'a t)
  | Return of 'a

and 'a tail =
  | Nil
  | Append of (unit -> 'a t) * 'a tail

let rec plug : type e. e t -> e tail -> e seq =
  fun t tail ->
    match t with
    | Empty -> force_tail tail
    | Return x -> Cons (x, tail)
    | Append (m, f) -> plug m (Append (f, tail))

and force_tail : type e. e tail -> e seq = function
  | Nil -> Done
  | Append (f, k) -> plug (f ()) k

let empty = Empty

let append m f = Append (m, f)

let to_seq t = plug t Nil

let rec of_list = function
  | [] -> Empty
  | x :: xs -> Append (Return x, fun () -> of_list xs)

let return x : _ t = Return x

let rec bind_seq seq ~f =
  match seq with
  | Done -> Empty
  | Cons (e, k) -> Append (f e, fun () -> bind_seq (force_tail k) ~f)

let bind t f =
  match t with
  (* speed hacks *)
  | Empty -> Empty
  | Return x -> f x
  (* general case *)
  | Append _ -> bind_seq (to_seq t) ~f

let map t ~f = bind t (fun a -> return (f a))

let rec iter_seq seq ~f =
  match seq with
  | Done -> ()
  | Cons (e, k) -> f e; iter_seq (force_tail k) ~f

let iter t ~f = iter_seq (to_seq t) ~f

let of_list = of_list

let to_list t =
  let q = Queue.create () in
  iter t ~f:(fun x -> Queue.enqueue q x);
  Queue.to_list q

let is_empty t =
  match to_seq t with
  | Done -> true
  | Cons _ -> false

let read_sexps cin =
  let rec loop () =
    match Option.try_with (fun () -> Sexp.input_sexp cin) with
    | None -> empty
    | Some s -> Append (return s, loop)
  in
  loop ()
