(* The purpose of the scout is to get us into the long game.
   What we do is build up an option to either
    - heal our slot 255 a lot
    - attack their slot 0 a lot
   Both of these strategies sacrifice the 0 and 1 slots. 

   The idea behind healing 255 a lot is that it's often the
   target of super-short fixed strategies, because it's the
   easiest slot to name from the opponent's context. These
   fixed strategies usually have fixed attack values, so if
   we heal the slot, they probably don't work.

   The idea behind attacking slot 0 is that it's often part
   of naive programs, setup phases, or super-short fixed
   strategies. If it's dead, these often fail.

   When this option is ready, we decide to take the disruptive
   aggressive strategy or the disruptive conservative strategy.

   How?
   - If slot 0 doesn't have anything in it, then we turtle.

   The scout should be done in the first 1000 (?) moves,
   and then never runs again.
*)

structure Scout :> DOMINATOR =
struct
  structure GS = GameState
  datatype src = datatype Kompiler.src

  structure EP = EmitProgram

  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  val lastmsg = ref ""
  val eprint =
      fn s => if s = !lastmsg
              then ()
              else (eprint ("[SCOUT] " ^ s ^ "\n"); lastmsg := s)

  fun create () =
  let

    fun prelude (enemy : int) : LTG.turn list =
        Macros.fastnum 2 enemy @ (* load enemy slot number into our slot 2 *)
        Macros.fastnum 0 8192    (* slot 0 holds 8192 (amt. of damage) *)

    fun proceed (action : LTG.card) : LTG.turn list =
        Macros.fastload 1 action @
        Macros.apply_slot_to_int 1 0 (* mine1 *) @
        Macros.apply_slot_to_slot 1 2 @
        Macros.apply_slot_to_slot 1 0 @ (* executes action mine1 enemy 8192 *)

        Macros.fastload 1 action @
        Macros.apply_slot_to_int 1 1 (* mine2 *) @
        Macros.apply_slot_to_slot 1 2 @
        Macros.apply_slot_to_slot 1 0 (* executes action mine2 enemy 8192 *)
(*
        Macros.fastload 1 Zombie @
        Macros.apply_slot_to_slot 1 2 @
        Macros.apply_slot_to_int 1 0 (* executes Zombie enemy 0 *)
*)

    fun opening_program slot =
      let
          (* too slow... *)
          val option =
              \"card" `
              ((\"twofivefive" `
                \"eightoneninetwo" `
                (* Use the card from slot 0 to 255, for 8192 damage/healing
                   (returns the identity) *)
                ($"card" -- Card LTG.Zero -- $"twofivefive" -- $"eightoneninetwo") --
                (* Then again from slot 1 to 255. *)
                ($"card" -- (Card LTG.Succ -- Card LTG.Zero) -- $"twofivefive" -- $"eightoneninetwo"))
               -- Int 255 -- Int 8192)

          (* Still too slow. *)
          val turtle =
              (\"twofivefive" `
                \"eightoneninetwo" `
                (Card LTG.Help -- Card LTG.Zero -- $"twofivefive" -- $"eightoneninetwo") --
                (* Then again from slot 1 to 255. *)
                (Card LTG.Help -- (Card LTG.Succ -- Card LTG.Zero) -- $"twofivefive" -- $"eightoneninetwo"))
              -- Int 255 -- Int 8192

          (* For doubleheal, should probably attempt revive if the
             slot dies... (this happens with gwillen's fastest_doubleshot). *)
          val doubleheal = prelude 255 @ proceed LTG.Help

          val doubletap = prelude 255 @ proceed LTG.Attack
              
          val prog = doubletap
      in
          (* eprint ("Opening: " ^ Kompiler.src2str prog);
             Kompiler.compile prog slot *)
          prog
      end handle (e as Kompiler.Kompiler s) =>
          let in
              eprint ("Kompilation failed: " ^ s ^ "\n");
              raise e
          end

    fun preview dos = ()

    datatype scoutmode =
        Start
      | Loading of LTG.turn list
      | Decide

    val slot = ref NONE
    val mode = ref Start

    val REQUIRED_SLOTS = [0, 1, 2]
    fun taketurn dos =
        let val gs = DOS.gamestate dos
            fun utoh () =
                let in
                    eprint "Scout can't run because it can't get slots 0 and 1 at the very start.";
                    eprint "This is bad! The scout is intended to run first.";
                    eprint "Suiciding...";
                    DOS.kill (DOS.getpid dos);
                    DOS.Can'tRun
                end
        in
            case !mode of
                Start =>
                  (case (DOS.reserve_fixed_slots dos REQUIRED_SLOTS, DOS.reserve_slot dos) of
                    (false, NONE) => utoh ()
                  | (true, NONE) => (app (DOS.release_slot dos) REQUIRED_SLOTS; utoh ())
                  | (false, SOME s) => (DOS.release_slot dos s; utoh ())
                  | (true, SOME s) =>
                        let
                            val prog = opening_program s
                        in
                            slot := SOME s;
                            eprint ("Opening program length: " ^ Int.toString (length prog) ^
                                    " to be put in slot " ^ Int.toString s);
                            mode := Loading prog;
                            taketurn dos
                        end)

              | Loading nil =>
                   let 
                   in
                     eprint ("Loading complete.");
                     mode := Decide;
                     taketurn dos
                   end

              | Loading (t :: rest) => 
                 let val myslot = valOf (!slot)
                     val am = LTG.slotisdead (GS.myside gs) myslot
                     (* XXX some of these temporaries aren't needed for the
                        whole program. *)
                     val a0 = LTG.slotisdead (GS.myside gs) 0
                     val a1 = LTG.slotisdead (GS.myside gs) 1
                     val a2 = LTG.slotisdead (GS.myside gs) 2
                 in
                     (* XXX actually decide *)
                     if am orelse a0 orelse a1 orelse a2
                     then 
                         let in
                             eprint ("Dead:" ^ (if am then " mine" else "") ^
                                     (if a0 then " 0" else "") ^
                                     (if a1 then " 1" else "") ^
                                     (if a2 then " 2" else "") ^
                                     " with " ^ Int.toString (length rest) ^
                                     " more moves...");
                             DOS.kill (DOS.getpid dos);
                             DOS.Can'tRun
                         end
                     else (mode := Loading rest; DOS.Turn t)
                 end

              | Decide =>
                 (* XXX this is not neeed any more ... *)
                 let val myslot = valOf (!slot)
                     val am = LTG.slotisdead (GS.myside gs) myslot
                     val a0 = LTG.slotisdead (GS.myside gs) 0
                     val a1 = LTG.slotisdead (GS.myside gs) 1
                 in
                     eprint ("Attacking!");
                     (* No matter what, we're done. *)
                     DOS.kill (DOS.getpid dos);

                     (* XXX actually decide *)
                     if am orelse a0 orelse a1
                     then
                         let in
                             eprint ("Dead:" ^ (if am then " mine" else "") ^
                                     (if a0 then " 0" else "") ^
                                     (if a1 then " 1" else ""));
                             DOS.Can'tRun
                         end
                     else (* DOS.Turn (LTG.RightApply (myslot, LTG.Help)) *)
                         DOS.Can'tRun
                 end
        end
  in
    { preview = preview,
      taketurn = taketurn }
  end
end

