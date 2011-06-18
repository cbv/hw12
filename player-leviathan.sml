
structure Leviathan :> LAYER =
struct
  open LTG;
  open Kompiler;

  structure GS = GameState


  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b



  val myslot0 = 1
  val myslot1 = 2
  val helpamount = 2048
  val hurtamount = 512
  val targetpointer = 4

(*
  val killer = 
      fix (\ "f" 
           (\ "_" ( ((Card Help) -- (Int myslot0) -- (Int myslot1) -- (Int helpamount)) 
                   -- ((Card Help) -- (Int myslot1) -- (Int myslot0) -- (Int helpamount)) 
                   -- ((Card Attack) -- (Int myslot1) 
                                     -- (Int myslot1) (* ((Card (Get)) -- (Int(targetpointer ))) *)
                                     -- (Int hurtamount))
                   -- ($ "f" -- Int(0) ))))

*)

 
    

  val fnslot = 0
  val target = ref 0

  (* applies f[1] to f[0], putting result in f[1] *)
  val applyregs = [LeftApply (K, 1),
                   LeftApply (S, 1),
                   RightApply (1, Get),
                   RightApply (1, Zero)]

  (* applies (S f[1]) to f[0], putting result in f[1] *)
  val sapplyregs =  LeftApply(S,1)  :: applyregs
                   

  (* copies f[1] to f[n] *)
  fun copyregs1 n = [LeftApply (Put, n),
                    RightApply (n, Zero),
                    LeftApply (Succ, n),
                    LeftApply (Get, n)]

  (* copies f[2] to f[n] *)
  fun copyregs2 n = [LeftApply (Put, n),
                     RightApply (n, Zero),
                     LeftApply (Succ, n),
                     LeftApply (Succ, n),
                     LeftApply (Get, n)]


  val instructions = ref `
                     [RightApply (0, Dec),
                      RightApply (1, Dec)]
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs 
                     @ ( copyregs1 0)
                     @ sapplyregs 
                     @ ( copyregs1 0)
                     @ sapplyregs 
                     @ ( copyregs1 0)
                     @ sapplyregs 
                     @ ( copyregs1 0)
                     @ sapplyregs 
                     @ ( copyregs1 0)
                     @ sapplyregs 
                    (* once it's constructed, move it to f[2] *)
                     @ ( copyregs1 2)
                     

                  
                  
                   
                   
                      
  val target = ref 0

  fun init gs = ()

  fun taketurn gs =
      let
          val theirside = GS.theirside gs
          val health = Array.sub (#2 theirside, 255 - !target)
          val _ = if health <= 0 then target := (!target) + 1 else ()
          val () = if List.null (!instructions)    
                   then instructions := ((compile (Int (!target)) 0) @ (copyregs2 1) @ applyregs)
                   else ()
          val (ins,inses) = (List.hd (!instructions), List.tl (!instructions))
          val _ = instructions := inses
      in
          ins
      end


end

structure Player = LayerFn(Leviathan)
