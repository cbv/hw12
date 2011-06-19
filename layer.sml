
structure GameState :> GAMESTATE =
struct

  exception GameState of string

  type stats = LTG.stats

  datatype gamestate = 
      G of { first_player : bool ref,
             is_my_turn : bool ref,
             my_side : LTG.side,
             their_side : LTG.side,
             (* TODO number of turns, scores, etc. *)
             my_stats : stats,
             their_stats : stats,
             time : int ref
             }

  fun first_player (G { first_player = ref b, ... }) = b

  fun initialstate fp =
      G { first_player = ref fp,
          is_my_turn = ref fp,
          my_side = LTG.initialside (),
          their_side = LTG.initialside (),
          my_stats = LTG.initialstats (),
          their_stats = LTG.initialstats (),
          time = ref 0}

  fun mystats (G { my_stats, ... }) = my_stats
  fun theirstats (G { their_stats, ... }) = their_stats

  fun myside (G { my_side, ... }) = my_side
  fun theirside (G { their_side, ... }) = their_side

  fun time (G { time, ... }) = !time

  fun printstats stats =
      TextIO.output (TextIO.stdErr, LTG.statstostring stats ^ "\n")

  (* TODO: Could also count how often the number of the slot has appeared
     in a cell? *)
  fun scoreopponentslot_withdistance (G { their_side, their_stats, ... }) dist idx =
      let val s = LTG.statfor their_stats idx

          (* Bonus points if easily addressed,
             mostly to break ties when cleaning up unused slots. *)
          val bonus = 256.0 - 4.0 * real (dist idx)
      in
       (* XXX weighted! *)
       if LTG.slotisdead their_side idx
       then ~1000.0
       else bonus + 
            real (LTG.stat_left_applications s) +
            real (LTG.stat_right_applications s) +
            LTG.stat_damage_done s +
            LTG.stat_healing_done s +
            real (LTG.stat_iterations s) +
            50.0 * real (LTG.stat_gotten s)
      end

  fun scoreopponentslot gs idx = scoreopponentslot_withdistance gs 
      (fn i => Numbers.naive_cost (255 - i)) idx

  (* XXX These need to take care of the number of
     turns, ending conditions (?), etc. *)
  fun my_turn (G { is_my_turn = myturn as ref true,
                   my_side, their_side, 
                   my_stats, their_stats, time, ... }) turn =
      let in
          LTG.taketurnex ((my_side, SOME my_stats), 
                          (their_side, SOME their_stats)) turn;
          myturn := false;
          time := !time + 1
      end
    | my_turn _ _ = raise GameState "it's not my turn!"
                   
  fun their_turn (G { is_my_turn = myturn as ref false,
                      my_side, their_side, 
                      my_stats, their_stats, time, ... }) turn =
      let in
          (* (with perspective swapped) *)
          LTG.taketurnex ((their_side, SOME their_stats),
                          (my_side, SOME my_stats)) turn;
          myturn := true;
          time := !time + 1
      end
    | their_turn _ _ = raise GameState "it's not their turn!"


end

functor LayerFn(L : LAYER) :> PLAYER =
struct
  structure GS = L.GS
  type state = GS.gamestate


  (* The PLAYER init function gets an optional turn
     which tells us whether we were player 0, and
     also something to replay. *)
  fun init NONE =
      let val state = GS.initialstate true
          val turn = L.taketurn state
      in 
          GS.my_turn state turn;
          (turn, state)
      end
    | init (SOME oturn) =
      let val state = GS.initialstate false
          val () = GS.their_turn state oturn
          val turn = L.taketurn state
      in 
          GS.my_turn state turn;
          (turn, state)
      end

  fun round (oturn, state) =
      let
          val () = GS.their_turn state oturn
          val turn = L.taketurn state
      in
          GS.my_turn state turn;
          (turn, state)
      end

end

functor CoroLayerFn(L : CORO_LAYER) :> LAYER =
struct
  structure GS = L.GS
  type state = GS.gamestate
  
  type return = L.return
  
  type 'a cont = 'a SMLofNJ.Cont.cont
  val callcc : ('a cont -> 'a) -> 'a = SMLofNJ.Cont.callcc
  val throw : 'a cont -> 'a -> 'b = SMLofNJ.Cont.throw
  
  val nextcont : (state * (LTG.turn cont)) cont option ref = ref NONE
  
  val n = ref 0
  
  fun init _ = ()
  fun taketurn gs =
    let in
      n := !n + 1;
      if !n mod 1000 = 0
      then (eprint ("Turn #" ^ Int.toString (!n) ^ ". Stats:\n");
            GS.printstats (GS.mystats gs);
            eprint "Theirs:\n";
            GS.printstats (GS.theirstats gs))
      else ();
      
      LTG.enable_trace false;
      
      callcc (fn (c : LTG.turn cont) =>
        let
          (* Recall that c is bound in the scope of the original callcc, so next
           * time we step into 'return', we return to the original c.  This is
           * why the outermost cont has to pass in the correct c.  *)
          fun return' (c' : LTG.turn cont) (m : LTG.turn) : return =
            let
              val (gs, c'') =
                callcc (fn (nc : (state * (LTG.turn cont)) cont) => (
                  nextcont := (SOME nc) ;
                  throw c' m
                ))
            in
              L.RETURN (gs, return' c'')
            end
        in
          (* Already running?  If so, pitch back; otherwise, kick off the
           * algorithm coro.  *)
          case !nextcont
          of SOME nc => throw nc (gs, c)
           | NONE => L.algorithm (gs, return' c)
        end
      )
    end
end
