structure Replicant :> LAYER =
struct
  structure GS = GameState
  structure K = Kompiler
  datatype src = datatype K.src
  datatype card = datatype Card.card

  val L = LTG.LeftApply
  val R = LTG.RightApply  

  fun init gs = ()

  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  datatype sum = LEFT | RIGHT
  fun sum (LEFT, a) = \ "left" ` \ "right" ` $"left" -- a
    | sum (RIGHT, a) = \ "left" ` \ "right" ` $"right" -- a
  fun pair (a, b) = \ "f" ` $"f" -- a -- b
  fun lst nil = sum (LEFT, Card I)
    | lst (h::t) = sum (RIGHT, pair (h, lst t))

  fun replicant {irep, idead, z, slot, bootslot, dead} =
  let
    fun doGetNext f = 
      \ "g" ` \ "list" ` $"list" -- Card I -- (\ "h" ` \ "t" ` (Card Put -- (f -- $"h")) -- ($"g" -- $"t"))
    fun app f = K.fix (doGetNext f)

    val drop = (* assumes $"rep" is in scope *)
      \ "j" ` (Card Zombie -- $"j" -- (* drop code *)
        (\ "I" ` z -- (Card Zombie -- Int irep -- (\ "I" ` $"rep")))) (* dropped code *)
    val rep' = \ "rep" ` \ "I" ` app drop -- (Card Get -- Int idead)
    val rep = K.fix rep'

    val bootstrap = Card Zombie -- Int bootslot -- (\ "I" ` Card Zombie -- Int irep -- rep)

    val pdead = K.compile (lst (map Int dead)) idead
    val pboot = K.compile bootstrap slot
  in pdead @ pboot
  end

  fun kill {j, slot} =
  let val attack = K.fix (\ "f" ` Card Put -- (Card Dec -- Int j) -- ($"f" -- Card I))
  in K.compile attack slot
  end

  val lame = \ "I" ` Card I

  (* config *)
  val enemytarget = 255
  val ourzombie = ref 0
  val theirzombies : int list ref = ref nil

  val turn = ref (kill {j=255-enemytarget, slot=0});
  val counter = ref 0;

  fun taketurn gs = 
  let
    fun f (t::ts) = (turn := ts; t)
      | f nil = L (I, 0)
    val (_, vits) = GS.myside gs
    fun showscores () = 
      if ((!counter) > 500) andalso ((!counter) < 800)
      then let
          val ballsmap = List.tabulate (8, fn x => (Int.toString (Array.sub (vits,x))))
        in
          eprint ("["^(String.concatWith "," ballsmap)^"]\n")
        end
      else ()
    val (_, theirvits) = GS.theirside gs
    val () = theirzombies := Array.foldri (fn (i, x, l) => x::l) nil theirvits
    val () = if length (!turn) > 0
             then ()
	     else if length (!theirzombies) = 0
	     then ()
	     else (case Array.findi (fn (i, x) => x = 0) theirvits
	     of NONE => ()
	      | SOME (ourdead, _) => 
	     turn := replicant {irep=ourdead, idead=5, (* :[ *)
	                        z=lame, slot=6, bootslot=hd (!theirzombies), dead=(!theirzombies)})
    val _ = counter := (!counter) + 1
    val _ = showscores ()
  in
    f (!turn)
  end
end

structure Player = LayerFn(Replicant)
