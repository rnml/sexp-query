include struct
  open Core.Std
  module List = List
  module Sexp = Sexp
  module Int = Int
  module Option = Option
  module Queue = Queue
  module Command = Command
  module Sexpable = Sexpable
  module String = String
  module type Stringable = Stringable

  let failwiths = failwiths

  include Int.Replace_polymorphic_compare

end
