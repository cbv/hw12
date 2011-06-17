structure Card = struct
   (* Cards are atomic values *)
   datatype card =
      I
    | zero
    | succ
    | dbl
    | get
    | put
    | S
    | K
    | inc
    | dec
    | attack
    | help
    | copy
    | revive
    | zombie
   
   (* Terms are partially applied terms and effects *)
   datatype term =
      Card of card
    | Int of IntInf.int
    | put1 of term
    | S1 of term
    | S2 of term * term
    | K1 of term
    | attack1 of term
    | attack2 of term * term
    | help1 of term
    | help2 of term * term
    | zombie1 of term
end

(* Primitive immutable playing field *)
structure Field :> sig
   type vitality = int
   type field

   exception OutOfRange

   val empty: field

   (* There are 256 slots, numbered 0-255 *)
   val get: field -> int -> Card.term * vitality
   val put: field -> int -> Card.term * vitality -> field

   (* The term component of a slot *)
   val getT: field -> int -> Card.term
   val putT: field -> int -> Card.term -> field

   (* The vitality component of a slot *)
   val getV: field -> int -> vitality
   val putV: field -> int -> vitality -> field
end = struct
   structure Map =
   SplayMapFn(struct type ord_key = int val compare = Int.compare end)

   type vitality = int
   type field = (Card.term * vitality) Map.map

   exception OutOfRange

   val empty = Map.empty

   fun get m x =
      if x > 255 orelse x < 0 then raise OutOfRange
      else case Map.find (m, x) of
              NONE => (Card.Card Card.I, 10000)
            | SOME (term, vit) => (term, vit)

   fun put m x (term, vit) =
      if x > 255 orelse x < 0 then raise OutOfRange
      else Map.insert (m, x, (term, vit))

   fun getT m x = #1 (get m x)
   fun getV m x = #2 (get m x)

   fun putT m x term = put m x (term, getV m x)
   fun putV m x vit = put m x (getT m x, vit)
end

structure Play:> sig
  type state = {proponent: Field.field, opponent: Field.field}
  val apply: state * Card.term * Card.term -> state * Card.term option
end = struct
  open Card

  infix >>=
  fun (state, x) >>= f = 
     case x of 
        NONE => (state, NONE)
      | SOME x => f (state, x)

  type state = {proponent: Field.field, opponent: Field.field}

  fun getInt (Int i) = SOME i
    | getInt (Card zero) = SOME 0
    | getInt _ = NONE

  fun apply (state as {proponent, opponent}, function, arg) = 
     case function of
        Card I => (state, SOME arg)

      | Int _ => (state, NONE)

      | Card succ => 
        (state, getInt arg) >>= (fn (state, x) =>
        (state, SOME (Int (x + 1))))

      | Card dbl => 
        (state, getInt arg) >>= (fn (state, x) => 
        (state, SOME (Int (x * 2))))

      | Card get =>
        (state, getInt arg) >>= (fn (state, x) =>
        (state, SOME (Field.getT proponent (IntInf.toInt x))
                handle Field.OutOfRange => NONE))

      | Card put => (state, SOME (put1 arg))
      | put1 _ => (state, SOME arg)

      | Card S => (state, SOME (S1 arg))
      | S1 term => (state, SOME (S2 (term, arg)))
      | S2 (f, g) => 
        apply (state, f, arg) >>= (fn (state, h) => 
        apply (state, g, arg) >>= (fn (state, y) =>
        apply (state, h, y)))

      | Card K => (state, SOME (K1 arg))
      | K1 term => (state, SOME term)

      | Card attack => (state, SOME (attack1 arg))

end
