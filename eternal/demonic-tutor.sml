structure DemonicTutor = struct

structure PFS = Posix.FileSys
structure PP = Posix.Process
structure PIO = Posix.IO

fun err x = TextIO.output (TextIO.stdErr, x ^ "\n")
fun debug x = () (* TextIO.output (TextIO.stdErr, x ^ "\n") *)

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

fun getExe base = 
   let 
      val filename = OS.Path.joinDirFile {dir = OS.FileSys.getDir (),
                                          file = "player-" ^ base ^ ".exe"}
   in
      filename
   end

fun setupPlayer player arg state = 
   let 
      val exe = getExe player

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
          outstream = mkOutstream outServer,
          state = state}
   end

type process = {pid: PIO.pid, 
                instream: TextIO.instream,
                outstream: TextIO.outstream,
                state: LTG.side}

fun winner (proponent, opponent) = false

fun report (n, proponent, opponent) =
   if n mod 20000 <> 0 then () else
   let
      fun playerData player = 
         let 
            val vita = #2 (#state player) 

            val (vitality, live, dead, zombie) = Array.foldr 
               (fn (x, (vitality, live, dead, zombie)) => 
                   if x > 0 
                   then (vitality + IntInf.fromInt x, live + 1, dead, zombie)
                   else if x = 0
                   then (vitality, live, dead + 1, zombie)
                   else (vitality, live, dead, zombie + 1)) (0, 0, 0, 0) vita
         in
            print ("DEAD: " ^ Int.toString dead)
            ; print (" / ZOMB: " ^ Int.toString zombie)
            ; print (" / LIVE: " ^ Int.toString live 
                     ^ " (average vitality of living: " 
                     ^ IntInf.toString (vitality div IntInf.fromInt live) 
                     ^ ")\n")
         end

      val (player0, player1) = 
         if n mod 2 = 0 then (proponent, opponent) else (opponent, proponent)
   in 
      print ("After " ^ Int.toString (n div 2) ^ " turns...\n")
      ; print "PLAYER 0 -- "
      ; playerData player0
      ; print "PLAYER 1 -- "
      ; playerData player1
   end

fun continue (n, proponent: process, opponent: process) =
   if n = 200000 orelse winner (proponent, opponent)
   then (report (n, proponent, opponent); print "Done.\n")
   else let
      val () = if n = 0 orelse n mod 20000 <> 0 then ()  
               else report (n, proponent, opponent) 
      (* val _ = TextIO.inputLine TextIO.stdIn *)

      (* GET THE MOVE *)
      val () = debug "Receiving in tutor"
      val play = LTGParse.rcv (#instream proponent)
      val () = debug "Done receiving in tutor"

      (* SEND THE MOVE *)
      val () = debug "Sending in tutor"
      val () = LTGParse.send (#outstream opponent) play
      val () = debug "Done sending in tutor"

      (* DO YOU WANT TO PLAY A GAME? *)
      val () = LTG.taketurn (#state proponent, #state opponent) play
   in
      continue (n+1, opponent, proponent)
   end

(* args are the result of Params.docommandline *)
fun go args = 
   let 
      val () = print "Starting...\n"
      val state0 = LTG.initialside ()
      val state1 = LTG.initialside ()
      val (process0, process1) = 
         case args of 
            [ player0, player1 ] => 
            (setupPlayer player0 "0" state0, 
             setupPlayer player1 "1" state1)
          | [] => 
            (err ("No main argument given!")
             ; err ("Usage: " ^ CommandLine.name () ^ " player0 player1")
             ; err ("playerN should be \"foo\" for \"player-foo.exe\"")
             ; OS.Process.exit OS.Process.failure) 
          | _ =>
            (err ("Wrong number of arguments given!")
             ; err ("Usage: " ^ CommandLine.name () ^ " player0 player1")
             ; err ("playerN should be \"foo\" for \"player-foo.exe\"")
             ; OS.Process.exit OS.Process.failure)
   in
      continue (0, process0, process1)
   end handle LTGParse.LTGIO s => (err ("Error: " ^ s)
                                   ; OS.Process.exit OS.Process.failure)

val () = go (Params.docommandline ())

end
