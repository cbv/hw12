structure LTGParse = struct

fun err x = TextIO.output (TextIO.stdErr, x ^ "\n")
fun debug x = TextIO.output (TextIO.stdErr, x ^ "\n")

val cardcard = 
   [ ("I", LTG.I),
     ("zero", LTG.Zero),
     ("succ", LTG.Succ),
     ("dbl", LTG.Dbl),
     ("get", LTG.Get),
     ("put", LTG.Put),
     ("S", LTG.S),
     ("K", LTG.K),
     ("inc", LTG.Inc),
     ("dec", LTG.Dec),
     ("attack", LTG.Attack),
     ("help", LTG.Help),
     ("copy", LTG.Copy),
     ("revive", LTG.Revive),
     ("zombie", LTG.Zombie) ]

fun parse card = 
   Option.map #2 (List.find (fn (s, _) => card = s) cardcard)

fun parseWhite card =
   case map parse (String.tokens Char.isSpace card) of
      [ card ] => card
    | _ => NONE (* Too few or too many tokens *)

fun str x = 
   #1 (valOf (List.find (fn (_, y) => x = y) cardcard))

exception LTGIO of string

fun rcv instream = 
   let 
      fun trim NONE = "nothing"
        | trim (SOME x) = String.substring (x, 0, size x - 1)

      val () = debug "Receiving"
      val move = TextIO.inputLine instream
      val () = debug ("Received " ^ trim move)
      val fst = TextIO.inputLine instream
      val () = debug ("Received " ^ trim fst)
      val snd = TextIO.inputLine instream
      val () = debug ("Received " ^ trim snd)
   in
      case (move, fst, snd) of 
         (SOME "1\n", SOME card, SOME slot) =>
         (case (parseWhite card, Int.fromString slot) of
             (SOME card, SOME slot) => LTG.LeftApply (card, slot)
           | (NONE, _) => raise LTGIO ("Not a card: " ^ card)
           | (_, NONE) => raise LTGIO ("Not a slot number: " ^ slot))

       | (SOME "2\n", SOME slot, SOME card) =>
         (case (Int.fromString slot, parseWhite card) of
             (SOME slot, SOME card) => LTG.RightApply (slot, card)
           | (NONE, _) => raise LTGIO ("Not a slot number: " ^ slot)
           | (_, NONE) => raise LTGIO ("Not a card: " ^ card))

       | (SOME arg, SOME _, SOME _) => 
         raise LTGIO ("Bad left/right argument: " ^ arg)

       | _ => 
         raise LTGIO ("Broken pipe?")
   end

fun send outstream move = 
   case move of 
      LTG.LeftApply (card, slot) =>
      (debug ("Sending left apply")
       ; TextIO.output (outstream, "1\n")
       ; err ("Sent: 1")
       ; TextIO.output (outstream, str card ^ "\n")
       ; err ("Sent: " ^ str card)
       ; TextIO.output (outstream, Int.toString slot ^ "\n")
       ; err ("Sent: " ^ Int.toString slot ^ "\n"))
    | LTG.RightApply (slot, card) => 
      (debug ("Sending left apply")
       ; TextIO.output (outstream, "2\n")
       ; err ("Sent: 2")
       ; TextIO.output (outstream, Int.toString slot ^ "\n")
       ; err ("Sent: " ^ Int.toString slot)
       ; TextIO.output (outstream, str card ^ "\n")
       ; err ("Sent: " ^ str card))

end
     
