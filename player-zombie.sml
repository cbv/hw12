structure Player :> PLAYER =
struct
open Kompiler

type state = LTG.turn list

(* zombiefy n zombiefies slot (get n+1) *)
fun zombifier n = 
    (compile
         (Lambda ("z",
                  Apply(
                  Apply(Card Card.Zombie,
                        Apply(Card Card.Get,
                              Apply (Card Card.Succ, Var "z"))), Var "z")))
         n)

fun load_num n i = compile (Int n) i

fun init _ = 
    let 
      (* Compile 7 to slot 1 *)
      val seven = load_num 7 1
            
      (* Compile zombiefy to slot 13 *)
      val zombiefy = zombifier 13
      val z = seven @ zombiefy
    in
      (hd z, tl z)
    end

fun round (_, p) = 
    if null p then (LTG.LeftApply (LTG.Zero, 0), [])
    else (hd p, tl p)

end
