signature KOMPILER =
sig

  exception Kompiler of string

  datatype src =
      Var of string
    | Lambda of string * src
    | Apply of src * src
    | Card of Card.card
    | Int of int

  (* a fixed point combinator, e.g.
      compile (fix (Lambda ("self", Lambda ("x", ...)))) slot
  *)
  val fix : src -> src

  (* compiles a source expression into a turn-list that will create
    that expression in the given slot *)
  val compile : src -> int -> LTG.turn list

  (* run the tests and print results *)
  val test : unit -> unit

end
