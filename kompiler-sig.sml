signature KOMPILER =
sig

  datatype src =
      Var of string
    | Lambda of string * src
    | Apply of src * src
    | Card of Card.card
  (* TODO: Add "Slot of int" ? Or should there be a combinator for that?
      Same for "Number of int" *)

  (* compiles a source expression into a turn-list that will create
    that expression in the given slot *)
  val compile : src -> int -> LTG.turn list

  (* run the tests and print results *)
  val test : unit -> unit

end
