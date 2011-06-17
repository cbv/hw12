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
   type vitality = IntInf.int
   type field

   exception OutOfRange

   val empty: field

   (* There are 256 slots, numbered 0-255 *)
   val get: field -> int -> Card.term * vitality
   val put: field -> int -> Card.term * vitality -> field

   (* The term component of a slot. NONE if out of range. *)
   val getT: field -> int -> Card.term option
   val putT: field -> int -> Card.term -> field

   (* The vitality component of a slot. NONE if out of range *)
   val getV: field -> int -> vitality option
   val putV: field -> int -> vitality -> field
end = struct
   structure Map =
   SplayMapFn(struct type ord_key = int val compare = Int.compare end)

   type vitality = IntInf.int
   type field = (Card.term * vitality) Map.map

   exception OutOfRange

   val empty = Map.empty

   fun get (m: field) x =
      if x > 255 orelse x < 0 then raise OutOfRange
      else case Map.find (m, x) of
              NONE => (Card.Card Card.I, 10000)
            | SOME (term, vit) => (term, vit)

   fun put m x (term, vit:vitality) =
      if x > 255 orelse x < 0 then raise OutOfRange
      else Map.insert (m, x, (term, vit))

   fun getT m x = SOME (#1 (get m x)) handle OutOfRange => NONE

   fun getV m x = SOME (#2 (get m x)) handle OutOfRange => NONE

   fun putT m x term = put m x (term, #2 (get m x))
   fun putV m x vit = put m x (#1 (get m x), vit)
end

structure Play:> sig
  type state = {proponent: Field.field, opponent: Field.field}
  val apply: bool -> state * Card.term * Card.term -> state * Card.term option
end = struct
  open Card

  infix >>=
  fun (state, x) >>= f = 
     case x of 
        NONE => (state, NONE)
      | SOME x => f (state, x)

  type state = {proponent: Field.field, opponent: Field.field}

  fun w_proponent (state: state) proponent = 
     {proponent = proponent, opponent = #opponent state}

  fun w_opponent (state: state) opponent = 
     {proponent = #proponent state, opponent = opponent}

  fun getInt (Int i) = SOME i
    | getInt (Card zero) = SOME 0
    | getInt _ = NONE

  local
  fun getIndex' flip x = 
     case getInt x of 
        NONE => NONE
      | SOME x => 
        if x < 0 orelse x > 255 
        then NONE 
        else if flip 
        then SOME (255 - IntInf.toInt x)
        else SOME (IntInf.toInt x)
  in
  val getIndex = getIndex' false
  val getIndexFLIP = getIndex' true
  end

  fun deIntInf x = 
     if x > 255 orelse x < 0 then NONE else SOME (IntInf.toInt x)

  val VIT_MAX: IntInf.int = 65535

  fun doInc (amount: IntInf.int) (vit: IntInf.int) = 
     if vit > 0 andalso vit < VIT_MAX
     then if vit + amount > VIT_MAX then VIT_MAX else vit + amount
     else vit

  fun doDec (amount: IntInf.int) (vit: IntInf.int) = 
     if vit > 0 andalso vit < VIT_MAX
     then if vit - amount < 0 then 0 else vit - amount
     else vit

  fun apply isZombie (state, function, arg) = 
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
        (state, getIndex arg) >>= (fn (state, x) =>
        (state, Field.getT (#proponent state) x))

      | Card put => (state, SOME (put1 arg))
      | put1 _ => (state, SOME arg)

      | Card S => (state, SOME (S1 arg))
      | S1 term => (state, SOME (S2 (term, arg)))
      | S2 (f, g) => 
        apply isZombie (state, f, arg) >>= (fn (state, h) => 
        apply isZombie (state, g, arg) >>= (fn (state, y) =>
        apply isZombie (state, h, y)))

      | Card K => (state, SOME (K1 arg))
      | K1 term => (state, SOME term)

      | Card inc => 
        (state, getIndex arg) >>= (fn (state, x) =>
        (state, Field.getV (#proponent state) x) >>= (fn (state, vit) =>
        (w_proponent state
            (Field.putV (#proponent state) x 
               (if isZombie then doDec 1 vit else doInc 1 vit)), 
         SOME (Card I))))

      | Card dec => 
        (state, getIndex arg) >>= (fn (state, x) => 
        (state, Field.getV (#opponent state) (255-x)) >>= (fn (state, vit) =>
        (w_opponent state 
            (Field.putV (#opponent state) x 
               (if isZombie then doInc 1 vit else doDec 1 vit)), 
         SOME (Card I))))

      | Card attack => (state, SOME (attack1 arg))
      | attack1 term1 => (state, SOME (attack2 (term1, arg)))
      | attack2 (i, j) => 
        (state, getIndex i) >>= (fn (state, attacker) =>
        (state, Field.getV (#proponent state) attacker) >>= (fn (state, vita) =>
        (state, getInt arg) >>= (fn (state, damage) =>
        if damage > vita then (state, NONE)
        else (w_proponent state 
                 (Field.putV (#proponent state) attacker (vita - damage)),
              getIndexFLIP j) >>= (fn (state, defender) =>
        (state, Field.getV (#opponent state) defender) >>= (fn (state, vitd) =>
        (w_opponent state
            (Field.putV (#opponent state) defender 
                (if isZombie 
                 then doInc ((damage * 9) div 10) vitd
                 else doDec ((damage * 9) div 10) vitd)),
         SOME (Card I)))))))

      | Card help => (state, SOME (help1 arg))
      | help1 term1 => (state, SOME (help2 (term1, arg)))
      | help2 (i, j) =>
        (state, getIndex i) >>= (fn (state, doctor) =>
        (state, Field.getV (#proponent state) doctor) >>= (fn (state, vitd) =>
        (state, getInt arg) >>= (fn (state, undamage) =>
        if undamage > vitd then (state, NONE)
        else (w_proponent state
                 (Field.putV (#proponent state) doctor (vitd - undamage)),
              getIndex j) >>= (fn (state, patient) =>
        (state, Field.getV (#proponent state) patient) >>= (fn (state, vitp) =>
        (w_proponent state
            (Field.putV (#proponent state) patient
                (if isZombie
                 then doDec ((undamage * 11) div 10) vitp
                 else doInc ((undamage * 11) div 10) vitp)),
         SOME (Card I)))))))

      | Card copy => 
        (state, getIndex arg) >>= (fn (state, x) =>
        (state, Field.getT (#opponent state) x))

      | Card revive =>
        (state, getIndex arg) >>= (fn (state, x) =>
        (state, Field.getV (#proponent state) x) >>= (fn (state, vit) =>
        if vit > 0 then (state, SOME (Card I))
        else (w_proponent state (Field.putV (#proponent state) x 1),
              SOME (Card I))))

      | Card zombie => (state, SOME (zombie1 arg))
      | zombie1 i => 
        (state, getIndexFLIP arg) >>= (fn (state, corpse) =>
        (state, Field.getV (#opponent state) corpse) >>= (fn (state, vit) =>
        if vit > 0 then (state, NONE) 
        else (w_opponent state (Field.put (#opponent state) corpse (arg, ~1)), 
              SOME (Card I))))
end
