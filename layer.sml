
structure GameState :> GAMESTATE =
struct

  exception GameState of string

  datatype gamestate = 
      G of { first_player : bool ref,
             is_my_turn : bool ref,
             my_side : LTG.side,
             their_side : LTG.side
             (* TODO number of turns, scores, etc. *)
             }

  fun first_player (G { first_player = ref b, ... }) = b

  fun initialstate fp =
      G { first_player = ref fp,
          is_my_turn = ref fp,
          my_side = LTG.initialside (),
          their_side = LTG.initialside () }

(*   fun my_turn (G { is_my_turn = ref true, *)
                   
  fun my_turn _ = raise GameState "unimplemented"   
  fun their_turn _ = raise GameState "unimplemented"   

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
