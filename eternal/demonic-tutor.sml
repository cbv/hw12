structure DemonicTutor = struct

structure PFS = Posix.FileSys
structure PP = Posix.Process
structure PIO = Posix.IO

val flagQuiet  = Params.flag false
   (SOME ("--quiet", "Suppress most output")) "quiet"
fun printquiet s = if !flagQuiet then () else print (s ^ "\n")

val flagReport = Params.flag true
   (SOME ("--noreport", "Do not report to server")) "report"

val flagHelp = Params.flag false
   (SOME ("--help", "Print an informative help message")) "help"

val flagMode = Params.param "duel"
   (SOME ("--mode", "Tutor mode (duel, stress, costress, auto)")) "mode"

val flagPoll = Params.param "10000"
   (SOME ("--poll", "How often the state is reported (0=never)")) "poll"

val flagLogStats = Params.flag false
   (SOME ("--logstats", "Log application stats for each player")) "logstats"

val max = 200000
val flagFreq = ref 20000

(*
val flagPoll   = Params.param "poll" "How often outputs are reported" "20000"
val flagMode   = Params.param "mode" "Run mode (duel, stress, auto)" "duel"
*)

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

fun mkStatstream player0 player1 tag =
   let
      val player0 = 
         String.concatWith "-" (String.tokens (fn x => x = #":") player1) 
      val player1 = 
         String.concatWith "-" (String.tokens (fn x => x = #":") player1) 
      val filename = "graph-" ^ player0 ^ "-" ^ player1 ^ "-" ^ tag ^ ".dat"
   in
      if !flagLogStats then SOME (TextIO.openOut filename) else NONE
   end

fun setupPlayer player arg state statstream = 
   let 
      val exe = Make.getExe player

      (* Set up two pipes *)
      val {infd = inServer, outfd = outPlayer} = PIO.pipe ()
      val {infd = inPlayer, outfd = outServer} = PIO.pipe ()
   in
      case PP.fork () of 
         NONE => (* Set up the player's pipes, exec away *)
         let 
            val devnull = PFS.openf ("/dev/null", PFS.O_WRONLY, PFS.O.append) 
         in
            PIO.dup2 {old = inPlayer, new = PFS.stdin }
          ; PIO.dup2 {old = outPlayer, new = PFS.stdout }
          ; if not (!flagQuiet) then ()
            else PIO.dup2 {old = devnull, new = PFS.stderr }
          ; debug "Forking child..."
          ; PP.execp (exe, [ exe, arg ]) 
         end

       | SOME pid => (* Return the pipes to children *)
         (PIO.close outPlayer
          ; PIO.close inPlayer
          ; {name = player,
             broke = ref false,
             pid = pid, 
             instream = mkInstream inServer, 
             outstream = mkOutstream outServer,
             statstream = statstream,
             state = state})
   end

type process = {name: string,
                broke: bool ref,
                pid: PIO.pid, 
                instream: TextIO.instream,
                outstream: TextIO.outstream,
                statstream: TextIO.outstream option,
                state: LTG.side}

fun winner (proponent, opponent) = 
    let fun lost player = 
          not (Array.exists (fn x => x > 0) (#2 (#state player)))
    in lost proponent orelse lost opponent end

fun playerData player = 
    let 
        val vita = #2 (#state player) 
    in
        if !(#broke player) then (0, 0, 100000, 100000)
        else 
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

exception Broke of int * process * process

fun continue (n, proponent: process, opponent: process) =
   if n = max orelse winner (proponent, opponent)
   then (report (n, proponent, opponent); 
         if n mod 2 = 0 
         then (n, proponent, opponent)
         else (n, opponent, proponent))
   else let
      val () = if n = 0 orelse n mod !flagFreq <> 0 then ()  
               else report (n, proponent, opponent) 
      (* val _ = TextIO.inputLine TextIO.stdIn *)

      (* GET THE MOVE *)
      val () = debug "Receiving in tutor"
      val play = LTGParse.rcv (#instream proponent)
         handle LTGParse.LTGIO s => 
            if n mod 2 = 0 
            then (err ("PLAYER 0 FUCKED UP: " ^ s ^ " (bad send)")
                  ; #broke proponent := true
                  ; raise Broke (n, proponent, opponent))
            else (err ("PLAYER 1 FUCKED UP: " ^ s ^ " (bad send)")
                  ; #broke opponent := true
                  ; raise Broke (n, opponent, proponent))

      val () = debug "Done receiving in tutor"

      (* SEND THE MOVE *)
      val () = debug "Sending in tutor"
      val () = LTGParse.send (#outstream opponent) play
         handle LTGParse.LTGIO s => 
            if n mod 2 = 0 
            then (err ("PLAYER 1 FUCKED UP: " ^ s ^ " (bad recv)")
                  ; #broke opponent := true
                  ; raise Broke (n, proponent, opponent))
            else (err ("PLAYER 0 FUCKED UP: " ^ s ^ " (bad recv)")
                  ; #broke proponent := true
                  ; raise Broke (n, opponent, proponent))
      val () = debug "Done sending in tutor"

      (* MAYBE LOG SOME DATA ABOUT THE MOVE *)
      val () = LTG.application_count_hook := 
	       Option.map (fn ss => fn n => TextIO.output(ss, Int.toString n ^ "\n")) 
			  (#statstream proponent)

      (* DO YOU WANT TO PLAY A GAME? *)
      val () = LTG.taketurn (#state proponent, #state opponent) play

   in
      continue (n+1, opponent, proponent)
   end

fun usage stat = 
  (err ("Usage: "^CommandLine.name()^" [options] [--mode duel] arg arg")
   ; err ("       "^CommandLine.name()^" [options] --mode stress arg")   
   ; err ("       "^CommandLine.name()^" [options] --mode auto")
   ; err ("The argument 'foo' runs (building if needed) player-foo.exe")
   ; err ("The argument 'foo:12' runs (building if needed) player-foo-12.exe")
   ; err ("WARNING: ALWAYS CHECK IN BEFORE USING 'player:rev' arguments!\n")
   ; err (Params.usage ())
   ; OS.Process.exit stat)

(* Runs a single match between two players *)
fun match player0 player1 = 
   let 
      (* Setup state *)
      val () = print ("Preparing " ^ player0 ^ " vs " ^ player1 ^ "\n")
      val ss0 = mkStatstream player0 player1 "0" 
      val ss1 = mkStatstream player0 player1 "1" 
      val process0 = setupPlayer player0 "0" (LTG.initialside ()) ss0
      val process1 = setupPlayer player1 "1" (LTG.initialside ()) ss1

      (* Run *)
      val () = print ("Starting " ^ player0 ^ " vs " ^ player1 ^ "\n")
      val (rounds, final0, final1) = continue (0, process0, process1)
         handle Broke data => data
 
      (* Cleanup *)
      val () = PP.kill (PP.K_PROC (#pid process0), Posix.Signal.kill)
      val () = PP.kill (PP.K_PROC (#pid process1), Posix.Signal.kill)
      val () = TextIO.closeIn (#instream process0)
      val () = TextIO.closeOut (#outstream process0)
      val () = TextIO.closeIn (#instream process1)
      val () = TextIO.closeOut (#outstream process1)

      val tok = String.tokens (fn c => c = #":")
   in
      (* Potentially record output *)
      if not (!flagReport) then ()
      else case (tok player0, tok player1) of
         ([ name0, rev0 ], [ name1, rev1 ]) =>
         (case (Int.fromString rev0, Int.fromString rev1) of 
             (SOME rev0, SOME rev1) => 
             let
                val () = print "Reporting versioned match to server...\n"
                fun vit vit' live' = 
                   if live' = 0 then "0"
                   else IntInf.toString (vit' div IntInf.fromInt live')
                val (vit0, live0, dead0, zomb0) = playerData final0
                val (vit1, live1, dead1, zomb1) = playerData final1
                fun f x = 
                   print (RPC.rpc"http://R_E_D_A_C_T_E_D/arena/log.php" x
                          ^ "\n")
             in
                f [ ("player0", name0),
                    ("player0rev", Int.toString rev0),
                    ("player1", name1),
                    ("player1rev", Int.toString rev1),
                    ("rounds", Int.toString rounds),
                    ("dead0", Int.toString dead0),
                    ("dead1", Int.toString dead1),
                    ("zomb0", Int.toString zomb0),
                    ("zomb1", Int.toString zomb1),
                    ("vit0", vit vit0 live0),
                    ("vit1", vit vit1 live1) ]
             end
           | _ => ())
       | _ => () 
      ; printquiet "Done.\n\n"
   end handle Make.MakeFailed => print "BUILD FAILED. NOTHING CAN BE DONE.\n\n"
      
(* uses time as a substitute for randomness *)
fun pick n =
   IntInf.toInt (Time.toNanoseconds (Time.now ()) 
                 mod IntInf.fromInt (valOf Int.maxInt))
   mod n

(* Main function*)
(* args are the result of Params.docommandline *)
fun go args = 
   let
      (* Handle special command-line parameters and flags *)
      val () = 
         case Int.fromString (!flagPoll) of 
            NONE =>
            (err ("Bad argument to --poll: " ^ !flagPoll ^ "!")
             ; usage OS.Process.failure)
          | SOME 0 => flagFreq := max
          | SOME i => flagFreq := i * 2
      val () = if not (!flagHelp) then ()
               else usage OS.Process.success
   in
      case (!flagMode, args) of
         ("duel", [ player0, player1 ]) => match player0 player1

       | ("costress", [ player1 ]) => 
         let 
            val contestants = 
               String.tokens Char.isSpace
                  (RPC.rpc "http://R_E_D_A_C_T_E_D/arena/contestants.php" [])
         in 
            List.app (fn x => match x player1) contestants
         end

       | ("stress", [ player0 ]) => 
         let 
            val contestants = 
               String.tokens Char.isSpace
                  (RPC.rpc "http://R_E_D_A_C_T_E_D/arena/contestants.php" [])
         in 
            List.app (match player0) contestants
         end

       | ("auto", []) =>
         let
            fun tupleify str = 
               case String.tokens (fn x => x = #":") str of
                  [ a, b, c, d, e ] => (a, b, c, d, e)
                | _ => (err "match.php!"; OS.Process.exit OS.Process.failure)

            fun loop () = 
              let 
                (* Get candidates from RPC, filter for all the total noobs *)
                val candidates =   
                  map tupleify 
                    (String.tokens Char.isSpace
                      (RPC.rpc "http://R_E_D_A_C_T_E_D/arena/match.php" []))
                val zeroes = List.filter (fn x => "0" = #5 x) candidates

                (* Pick a random zero, or else a random low number *)           
                val (p0, r0, p1, r1, _) = 
                   if null zeroes 
                   then List.nth (candidates, pick (length candidates))
                   else List.nth (zeroes, pick (length zeroes))
              in
                match (p0 ^ ":" ^ r0) (p1 ^ ":" ^ r1); loop ()
              end
         in
            loop ()
         end

       | ("duel", _) => 
         (err "Wrong number of arguments (duel)"; usage OS.Process.failure)
       | ("stress", _) => 
         (err "Wrong number of arguments (stress)"; usage OS.Process.success)
       | ("auto", _) => 
         (err "Wrong number of argumetns (auto)"; usage OS.Process.success)
       | (mode, _) => 
         (err ("Invalid mode '" ^ mode ^ "'"); usage OS.Process.success)
   end handle LTGParse.LTGIO s => (err ("Error: " ^ s)
                                   ; OS.Process.exit OS.Process.failure)

val () = go (Params.docommandline ())

end
