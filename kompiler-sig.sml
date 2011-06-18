signature KOMPILER =
sig

  exception Kompiler of string

  datatype src =
      Var of string
    | Lambda of string * src
    | Apply of src * src
    | Card of Card.card
    | Int of int

  val src2str : src -> string

  (*
   Copy these into your file for shorter syntax, e.g.

    \x.f (\y. x x y)
                                              
    val w = \"x" ` $"f" -- (\"y" ` $"x" -- $"x" -- $"y")

  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b
   *)

  (* Helpful operators *)

  (* A fixed point combinator, e.g.
      compile (fix (Lambda ("self", Lambda ("x", ...)))) slot *)
  val fix : src -> src
  (* Takes an expression of type unit.  Wraps in a function that ignores its
     single argument, evaluates the expression, and then returns itself. *)
  val run_and_return_self : src -> src
  (* Take a function that represents the body of a loop, and return a function
    that, when called, executes one iteration of the loop. The argument passed
    the first time is used as the counter for the first iteration; after that
    the counter is incremented by one. *)
  val for : src -> src

  (* Compiles a source expression into a turn-list that will create
     that expression in the given slot. *)
  val compile : src -> int -> LTG.turn list
  (* As above, but doesn't clear the content of the slot first.  If the given
    turns are played, src will be left-applied to the current content of the
    slot. *)
  val compile_no_clear : src -> int -> LTG.turn list


  (* Internal utilities *)

  (* Kombinator internal language *)
  datatype kil = KApply of kil * kil
               | KCard of Card.card
               | KVar of string

  (* First translate from lambda calculus to Kombinators. *)
  val src2kil : src -> kil

  (* Run the tests and print results *)
  val test : unit -> unit
  val tomtest : unit -> unit

end
