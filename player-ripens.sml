
structure Ripens :> CORO_LAYER =
struct
  structure GS = GameState
  datatype src = datatype Kompiler.src
  
  datatype return = RETURN of (GS.gamestate * (LTG.turn -> return))
  
  fun eprint s = TextIO.output (TextIO.stdErr, s)

  (* Hard to kill. *)
  val ATTACK_SLOT = 0

  datatype mode =
      FindTarget
      (* Building the attack program for the given
         cell. *)
    | Emit of int * LTG.turn list
      (* Keep attacking this slot until it's dead,
         then find a new target. *)
    | Attacking of int

  val compare_scores = ListUtil.bysecond Real.compare


  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  (* Takes an expression of object type unit.
     Wraps in a function that ignores its single argument,
     evaluates the expression, and then returns itself. 

     fun f _ = (exp; f)
     *)
  fun returnself src =
      let val recursive =
          \"self" ` 
          \"unused" `
          Card LTG.Put -- src -- $"self"

          (* CBV fix-point operator. *)
          val minifix =
              \"x" ` $"f" -- (\"y" ` $"x" -- $"x" -- $"y")
          val fix = \"f" ` minifix -- minifix
      in
          Apply (fix, recursive)
      end

  (* Makes a program that attacks the target slot index,
     and then returns itself (so it sticks around). *)
  fun attackprogram target =
    let
        (* XXX should dec as much as we can in this turn,
           without exceeding 1000. *)

        (* Numbers are reversed when attacking opponent. *)
        val revtarget = 255 - target

        val dec = 
            (\"f" ` $"f" -- ($"f" -- ($"f" -- ($"f" -- $"f")))) --
            (\"_" ` Card LTG.Dec -- Int revtarget)
        val prog = returnself dec
    in
        Kompiler.compile prog ATTACK_SLOT
    end handle (e as Kompiler.Kompiler s) =>
        let in
            eprint ("Kompilation failed: " ^ s ^ "\n");
            raise e
        end

  val n = ref 0
  
  (* emit : LTG.turn list -> (GS.gamestate * (LTG.turn -> return))
  *                       -> (GS.gamestate * (LTG.turn -> return)) 
  **)
  fun emit nil (gs, return) = (gs, return)
    | emit (m::ms) (gs, return) =
      let
        val RETURN (gs, return) = return m
      in
        emit ms (gs, return)
      end
  
  fun algorithm (gs, return) =
    let
      val theirside = GS.theirside gs
      (* Find the highest-value slot in the
         opponent's state, according to the
         stats *)
      val stats = GS.theirstats gs
      val slots = List.tabulate (256, fn i =>
                                 (i, GS.scoreopponentslot gs i))

      (* Maybe should have a lower bound on what it will
         consider valuable, and just heal/revive if there
         are no current high-value targets. *)
      val (best, _) = ListUtil.max compare_scores slots
      
      val prog = attackprogram best
      (* val () = eprint ("Program: " ^ LTG.turns2str prog ^ "\n") *)
      val () = eprint ("Attack Program size: " ^ Int.toString (length prog) ^ "\n")
      
      val (gs, return) = emit prog (gs, return)
      
      fun attack (gs, return) =
        let
          val theirside = GS.theirside gs
          val health = Array.sub (#2 theirside, best)
        in
          if health <= 0
          then (eprint ("Success! Killed slot " ^
                        Int.toString best ^ "\n");
                (gs, return))
          else let
                 val RETURN (gs, return) = return (LTG.RightApply (ATTACK_SLOT, LTG.I))
               in
                 attack (gs, return)
               end
        end
        
        val (gs, return) = attack (gs, return)
      in
        algorithm (gs, return)
      end
end

structure Player = LayerFn(CoroLayerFn(Ripens))
