structure Killer :> LAYER =
struct
  structure GS = GameState
  structure K = Kompiler
  datatype src = datatype K.src
  
  (* This one doesn't pay any attention to the
     game state. *)
  fun init gs = ()

  fun eprint s = TextIO.output (TextIO.stdErr, s)


  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  val help1 = \"slota" ` Card LTG.Help -- $"slota" -- Int 7 -- (Card LTG.Get -- Int 1)
  val help2 = \"slotb" ` (Card LTG.Help -- (Card LTG.Succ -- $"slotb") -- Int 7-- (Card LTG.Get -- Int 1))
  val help = \"slotx" ` (Card LTG.S -- help1 -- help2) -- (Card LTG.Dbl -- $"slotx")
  val attack = \"slotc" ` Card LTG.Attack -- Int 7 -- $"slotc" -- (Card LTG.Get -- Int 2)
  val f = (Card LTG.S -- (Card LTG.Get -- Int 4) -- (Card LTG.Get -- Int 5))
  val m = \"x" ` (Card LTG.Put) -- (f -- $"x") -- (\"q" ` (Card LTG.Get -- $"x") -- ((Card LTG.S --
  (Card LTG.K -- Card LTG.Succ) -- (Card LTG.K -- $"x")) -- $"y"))
  val m = \"x" ` (Card LTG.Put) -- (f -- $"x") -- (\"q" ` (Card LTG.Get -- $"x") -- ((Card LTG.S --
  (Card LTG.K -- Card LTG.Succ) -- (Card LTG.K -- $"x")) -- $"y"))
  val bigm = \"m" ` \"x" ` (Card LTG.Put) -- (f -- $"x") -- 
  (\"q" ` $"m" -- (Card LTG.Succ -- $"x"))
  val fixbigm = K.fix bigm


  val p8 = (K.compile (Int 6000) 0)
  val p0 = (K.compile (Card LTG.Get -- Int 0) 8)
  val p1 = (K.compile (Card LTG.Get -- Int 8) 1)
  val p2 = (K.compile (Int 11200) 2)
  val phelp = (K.compile help 4)
  val pattack = (K.compile attack 5)
  val pmbig = (K.compile fixbigm 3)
  val pcompile = (K.compile (Card LTG.Get -- (Int 3)) 6) @ (List.tabulate (110,
  fn x => LTG.RightApply (6,LTG.Zero)))
  val p = p8 @ p0 @ p1 @ p2 @ phelp @ pattack @ pmbig @ pcompile
  val turn = ref p;
  val counter = ref 0;
  val _ = eprint (Int.toString (List.length p))
  fun taketurn gs = 
  let
    fun f (t::ts) = 
      let 
        val _ = turn := ts
      in
        t
      end
      | f nil = LTG.LeftApply (LTG.I, 0)
    val (_,vits) = GS.myside gs
    fun showscores () = 
      if ((!counter) > 720) andalso ((!counter) < 800)
      then 
        let
          val balls = List.tabulate (8,fn x=> x)
          val ballsmap = List.map (fn x => (Int.toString (Array.sub (vits,x)))) balls
          val ballsprint = "["^(String.concatWith "," ballsmap)^"]\n"
        in
          eprint ballsprint
        end
      else ()
    val _ = counter := (!counter) + 1
  in
    f (!turn)
  end
end

 
structure Player = LayerFn(Killer)
