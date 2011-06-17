structure PlayerIO = struct

open LTGParse

fun err x = TextIO.output (TextIO.stdErr, x ^ "\n")
fun debug x = TextIO.output (TextIO.stdErr, x ^ "\n")

val x = ref ""

fun continue (turn, state) = 
   let in
      debug ("Sending in player " ^ !x)
      ; send TextIO.stdOut turn
      ; debug ("Receiving in player " ^ !x) 
      ; continue (Player.round (rcv TextIO.stdIn, state))
   end

(* Start *)
val () = 
   let
      val args = Params.docommandline ()
      val seed =
         case args of 
            [ "0" ] => (debug "Player 0"
                        ; x := "0"
                        ; Player.init NONE)
          | [ "1" ] => (debug "Player 1"
                        ; x := "1"
                        ; Player.init (SOME (rcv TextIO.stdIn)))
          | [] => 
            (err ("No main argument given!")
             ; err ("Usage: " ^ CommandLine.name () ^ " {0, 1}")
             ; err ("0 is the first player, 1 is the second player")
             ; OS.Process.exit OS.Process.failure) 
          | [ s ] => 
            (err ("Bad argument: " ^ s)
             ; err ("Usage: " ^ CommandLine.name () ^ " {0, 1}")
             ; err ("0 is the first player, 1 is the second player")
             ; OS.Process.exit OS.Process.failure)
          | _ => 
            (err ("Bad argument or too many arguments given!")
             ; err ("Usage: " ^ CommandLine.name () ^ " {0, 1}")
             ; err ("0 is the first player, 1 is the second player")
             ; OS.Process.exit OS.Process.failure)
   in
      continue seed
   end handle LTGIO s => (err ("Error: " ^ s)
                          ; OS.Process.exit OS.Process.failure)
end
