structure Dungeon = struct

structure PFS = Posix.FileSys
structure PP = Posix.Process
structure PIO = Posix.IO

fun err x = TextIO.output (TextIO.stdErr, x ^ "\n")
fun debug x = TextIO.output (TextIO.stdErr, x ^ "\n")

fun mkInstream in_file_desc = 
   TextIO.mkInstream
    (TextIO.StreamIO.mkInstream 
     (PIO.mkTextReader {fd = in_file_desc, 
                        name = "Reader from the pipe",
                        initBlkMode = true}, ""))

fun mkOutstream out_file_desc = 
   TextIO.mkOutstream
    (TextIO.StreamIO.mkOutstream
     (PIO.mkTextWriter {fd = out_file_desc,
                        name = "Writer to the pipe",
                        initBlkMode = true,
                        chunkSize = 1024,
                        appendMode = true}, IO.NO_BUF))

fun setupPlayer exe arg = 
   let 
      (* Set up two pipes *)
      val {infd = inServer, outfd = outPlayer} = PIO.pipe ()
      val {infd = inPlayer, outfd = outServer} = PIO.pipe ()
   in
      case PP.fork () of 
         NONE => (* Set up the player's pipes, exec away *)
         let in
            PIO.dup2 {old = inPlayer, new = PFS.stdin }
          ; PIO.dup2 {old = outPlayer, new = PFS.stdout }
          ; debug "Forking child..."
          ; PP.execp (exe, [ exe, arg ]) 
         end

       | SOME pid => (* Return the pipes to children *)
         {pid = pid, 
          instream = mkInstream inServer, 
          outstream = mkOutstream outServer}
   end

type process = {pid: PIO.pid, 
                instream: TextIO.instream,
                outstream: TextIO.outstream}

fun report (n, play) =
   let 
      val swapped = n mod 2 = 1
   in
      print ("Turn " ^ Int.toString (n div 2 + 1))
      ; print (": player " ^ (if swapped then "1" else "0" ^ " ran "))
      ; case play of 
           LTG.LeftApply (card, slot) => 
           print (" card " ^ LTGParse.str card 
                  ^ " with argument of slot " ^ Int.toString slot)
         | LTG.RightApply (slot, card) =>
           print (" slot " ^ Int.toString slot 
                  ^ " with argument of card " ^ LTGParse.str card)
      ; print "\n" 
   end

fun continue (n, player: process, opponent: process) =
   if n = 200000 then () else
   let
      (* val _ = TextIO.inputLine TextIO.stdIn *)
      val () = debug "Receiving in tutor"
      val play = LTGParse.rcv (#instream player)
      val () = debug "Done receiving in tutor"
   in
      report (n, play)
      ; debug "Sending in tutor"
      ; LTGParse.send (#outstream opponent) play
      ; debug "Done sending in tutor"
      ; continue (n+1, opponent, player)
   end

(* args are the result of Params.docommandline *)
fun go args = 
   let 
      val () = print "Starting...\n"
      val (process1, process2) = 
         case args of 
            [ player1, player2 ] => 
            (setupPlayer player1 "0", setupPlayer player2 "1")
          | [] => 
            (err ("No main argument given!")
             ; err ("Usage: " ^ CommandLine.name () ^ " player1 player2")
             ; err ("Both players are names of executables")
             ; OS.Process.exit OS.Process.failure) 
          | _ =>
            (err ("Wrong number of arguments given!")
             ; err ("Usage: " ^ CommandLine.name () ^ " player1 player2")
             ; err ("Both players are names of executables")
             ; OS.Process.exit OS.Process.failure)
   in
      continue (0, process1, process2)
   end handle LTGParse.LTGIO s => (err ("Error: " ^ s)
                          ; OS.Process.exit OS.Process.failure)

val () = go (Params.docommandline ())

end
