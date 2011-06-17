structure LTGParse = struct

fun err x = TextIO.output (TextIO.stdErr, x ^ "\n")
fun debug x = () (* TextIO.output (TextIO.stdErr, x ^ "\n") *)

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

fun out (outstream, string) = (TextIO.output (outstream, string); TextIO.flushOut outstream)

fun send outstream move = 
   case move of 
      LTG.LeftApply (card, slot) =>
      (debug ("Sending left apply")
       ; out (outstream, "1\n")
       ; TextIO.flushOut outstream
       ; debug ("Sent: 1")
       ; out (outstream, str card ^ "\n")
       ; TextIO.flushOut outstream
       ; debug ("Sent: " ^ str card)
       ; out (outstream, Int.toString slot ^ "\n")
       ; TextIO.flushOut outstream
       ; debug ("Sent: " ^ Int.toString slot ^ "\n"))
    | LTG.RightApply (slot, card) => 
      (debug ("Sending left apply")
       ; out (outstream, "2\n")
       ; TextIO.flushOut outstream
       ; debug ("Sent: 2")
       ; out (outstream, Int.toString slot ^ "\n")
       ; TextIO.flushOut outstream
       ; debug ("Sent: " ^ Int.toString slot)
       ; out (outstream, str card ^ "\n")
       ; TextIO.flushOut outstream
       ; debug ("Sent: " ^ str card))

end
     
