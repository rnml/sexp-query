open Std_internal

let command =
  Command.basic ~summary:"sexp query"
    Command.Spec.(empty +> anon ("QUERY" %: Syntax.arg))
    (fun query () ->
       let inputs = Sequence.read_sexps stdin in
       let outputs = Query.run query inputs in
       Sequence.iter outputs ~f:(fun s ->
         print_endline (Sexp.to_string_hum s))
    )

let () = Command.run command
