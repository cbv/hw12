structure PlayerIO = struct

open LTGParse

fun err x = TextIO.output (TextIO.stdErr, x ^ "\n")

fun continue (turn, state) = 
   let in
      send TextIO.stdOut turn
      ; continue (Player.round (rcv TextIO.stdIn, state))
   end

(* Start *)
val () = 
   let
      val args = Params.docommandline ()
      val seed =
         case args of 
            [] => 
            (print ("No main argument given!\n")
             ; print ("Usage: " ^ CommandLine.name () ^ " {0, 1}\n")
             ; print ("0 is the first player, 1 is the second player\n")
             ; OS.Process.exit OS.Process.failure) 
          | [ "0" ] => Player.init NONE
          | [ "1" ] => Player.init (SOME (rcv TextIO.stdIn))
          | _ => 
            (print ("Bad argument or too many arguments given!\n")
             ; print ("Usage: " ^ CommandLine.name () ^ " {0, 1}\n")
             ; print ("0 is the first player, 1 is the second player\n")
             ; OS.Process.exit OS.Process.failure)
   in
      continue seed
   end handle LTGIO s => (err ("Error: " ^ s ^ "\n")
                          ; OS.Process.exit OS.Process.failure)
end
