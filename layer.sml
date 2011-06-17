
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
             their_stats : stats
             }

  fun first_player (G { first_player = ref b, ... }) = b

  fun initialstate fp =
      G { first_player = ref fp,
          is_my_turn = ref fp,
          my_side = LTG.initialside (),
          their_side = LTG.initialside (),
          my_stats = LTG.initialstats (),
          their_stats = LTG.initialstats () }

  fun mystats (G { my_stats, ... }) = my_stats
  fun theirstats (G { their_stats, ... }) = their_stats

  fun printstats stats =
      TextIO.output (TextIO.stdErr, LTG.statstostring stats ^ "\n")

  (* XXX These need to take care of the number of
     turns, ending conditions (?), etc. *)
  fun my_turn (G { is_my_turn = myturn as ref true,
                   my_side, their_side, 
                   my_stats, their_stats, ... }) turn =
      let in
          LTG.taketurnex ((my_side, SOME my_stats), 
                          (their_side, SOME their_stats)) turn;
          myturn := false
      end
    | my_turn _ _ = raise GameState "it's not my turn!"
                   
  fun their_turn (G { is_my_turn = myturn as ref false,
                      my_side, their_side, 
                      my_stats, their_stats, ... }) turn =
      let in
          (* (with perspective swapped) *)
          LTG.taketurnex ((their_side, SOME their_stats),
                          (my_side, SOME my_stats)) turn;
          myturn := true
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
