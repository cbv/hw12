structure DemonicTutor = struct

structure PFS = Posix.FileSys
structure PP = Posix.Process
structure PIO = Posix.IO

fun err x = TextIO.output (TextIO.stdErr, x ^ "\n")
fun debug x = () (* TextIO.output (TextIO.stdErr, x ^ "\n") *)

(* SETTING UP PIPES *)
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

fun setupPlayer player arg state = 
   let 
      val exe = Make.getExe player

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
         {name = player,
	  pid = pid, 
          instream = mkInstream inServer, 
          outstream = mkOutstream outServer,
          state = state}
   end

type process = {name: string,
		pid: PIO.pid, 
                instream: TextIO.instream,
                outstream: TextIO.outstream,
                state: LTG.side}

fun winner (proponent, opponent) = 
    let fun lost player = 
          not (Array.exists (fn x => x > 0) (#2 (#state player)))
    in lost proponent orelse lost opponent end

fun playerData player = 
    let 
        val vita = #2 (#state player) 
    in
	Array.foldr 
	    (fn (x, (vitality, live, dead, zombie)) => 
		if x > 0 
		then (vitality + IntInf.fromInt x, live + 1, dead, zombie)
		else if x = 0
		then (vitality, live, dead + 1, zombie)
		else (vitality, live, dead, zombie + 1)) (0, 0, 0, 0) vita
    end

(*
fun export (n, proponent, opponent) =
    (case !cardfax of 
	 SOME server =>
	 let 
	     val (_, plive, _, _) = playerData proponent
	     val (_, olive, _, _) = playerData opponent
	     fun defeat (w, l) =
		 let val s = "curl 'http://" ^ server ^ "/match?win=" ^ w ^ "&lose=" ^ l ^ "'\n"
		 in print s; OS.Process.system s; () end
	 in
	     case Int.compare (plive, olive) of
		 GREATER => defeat (#name proponent, #name opponent)
	       | LESS => defeat (#name opponent, #name proponent)
	       | EQUAL => ()
	 end
       | NONE => ()) *)

fun report (n, proponent, opponent) =
   let
       fun printPlayerData player =
	   let val (vitality, live, dead, zombie) = playerData player
	   in
         print ("DEAD: " ^ Int.toString dead)
	     ; print (" / ZOMB: " ^ Int.toString zombie)
	     ; if live = 0 then print " / LIVE: 0\n"
           else print (" / LIVE: " ^ Int.toString live 
		               ^ " (average vitality of living: " 
		               ^ IntInf.toString (vitality div IntInf.fromInt live) 
		               ^ ")\n")
	   end
      val (player0, player1) = 
         if n mod 2 = 0 then (proponent, opponent) else (opponent, proponent)
   in 
      print ("After " ^ Int.toString (n div 2) ^ " turns...\n")
      ; print "PLAYER 0 -- "
      ; printPlayerData player0
      ; print "PLAYER 1 -- "
      ; printPlayerData player1
   end

fun continue (n, proponent: process, opponent: process) =
   if n = 200000 orelse winner (proponent, opponent)
   then (report (n, proponent, opponent); 
         if n mod 2 = 0 
         then (n, proponent, opponent)
         else (n, opponent, proponent))
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
      fun initialize args = 
         case args of 
	        [ player0, player1 ] => 
            let 
               val t0 = String.tokens (fn c => c = #":") player0
               val t1 = String.tokens (fn c => c = #":") player1
               val report = 
               case (t0, t1, map Int.fromString t0, map Int.fromString t1) of
                 ([name0, _], [name1, _], [_, SOME rev0], [_, SOME rev1]) =>
                 SOME (name0, rev0, name1, rev1)
               | _ => NONE
            in
              (report, 
               setupPlayer player0 "0" state0, 
               setupPlayer player1 "1" state1)
            end
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
      val (report, process0, process1) = initialize args

      val (rounds, final0, final1) = continue (0, process0, process1)
   in
      case report of 
         NONE => print "Done."
       | SOME (name0, rev0, name1, rev1) => 
         let
            val () = print "Reporting versioned match to server...\n"
            fun vit vit' live' = 
               if live' = 0 then "0"
               else IntInf.toString (vit' div IntInf.fromInt live')
            val (vit0, live0, dead0, zombie0) = playerData final0
            val (vit1, live1, dead1, zombie1) = playerData final1
            val f = print o RPC.rpc "http://R_E_D_A_C_T_E_D/arena/log.php"
         in
            f [ ("player0", name0),
                ("player0rev", Int.toString rev0),
                ("player1", name1),
                ("player1rev", Int.toString rev1) ]
            ; print "\nDone.\n"
         end  
   end handle LTGParse.LTGIO s => (err ("Error: " ^ s)
                                   ; OS.Process.exit OS.Process.failure)

val () = go (Params.docommandline ())

end
