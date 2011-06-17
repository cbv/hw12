structure Card = struct

   (* Cards are atomic actions *)
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

   exception FieldSize

   val empty = Map.empty

   fun get m x =
      if x > 255 orelse x < 0 then raise FieldSize
      else case Map.find (m, x) of
              NONE => (Card.Card Card.I, 10000)
            | SOME (term, vit) => (term, vit)

   fun put m x (term, vit) =
      if x > 255 orelse x < 0 then raise FieldSize
      else Map.insert (m, x, (term, vit))

   fun getT m x = #1 (get m x)
   fun getV m x = #2 (get m x)

   fun putT m x term = put m x (term, getV m x)
   fun putV m x vit = put m x (getT m x, vit)

end

structure Play = struct



end
