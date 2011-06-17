val line = fn () => TextIO.inputLine TextIO.stdIn
val err = fn s => (TextIO.output (TextIO.stdErr, "Error: " ^ s ^ "\n")
                   ; OS.Process.exit OS.Process.failure)

val (revision, players) = 
  let
    val default = implode (List.tabulate (72, fn _ => #"-")) ^ "\n"

    (* Get dumb first line *)
    val () = if line () = SOME default then () else err "first line bad"
  
    (* Revision on second line *)
    val revision = 
      let val newline = line () in
        if newline = NONE then err "second line bad"
        else case String.tokens Char.isSpace (valOf newline) of 
          rev :: "|" :: _ :: "|" :: _ =>
          (case Int.fromString (String.substring (rev, 1, size rev - 1)) of
             NONE => err "revision bad"  
           | SOME rev => rev)
        | _ => err "second line bad"
       end

    (* Get dumb third line *)
    val () = if line () = SOME "Changed paths:\n" then ()
             else err "third line bad"

    fun loop ss = 
      let val newline = line () in
        if newline = SOME default 
        then ss
        else if newline = NONE
        then err "unexpected end of file"
        else case String.tokens (not o Char.isAlphaNum) (valOf newline) of
          _ :: "hw12" :: "player" :: "sig" :: "sml" :: [] => loop ss
        | "A" :: "hw12" :: "player" :: s :: "sml" :: [] => loop (s :: ss)
        | "M" :: "hw12" :: "player" :: s :: "sml" :: [] => loop (s :: ss)
        | _ => loop ss
      end
  in
    (revision, rev (loop []))
  end

val () = if null players then () 
         else print ("Revision " ^ Int.toString revision ^ ":\n"
                     ^ String.concatWith "\n" players ^ "\n")
  
val _ =
  let 
    val m = MySQL.connect "root" (SOME "R_E_D_A_C_T_E_D")
      handle e as MySQL.MySQL s => (print ("Error: " ^ s ^ "\n"); raise e)

    fun query s = 
       MySQL.query m s handle MySQL.MySQL s =>
       		    (print ("Error: " ^ s ^ "\n")
                     ; OS.Process.exit OS.Process.failure)

    val () = Option.app MySQL.free (query "use hw12")

    fun insert player = 
      let 
        val res = query ("insert into players values (NULL, \""
                         ^ player ^ "\", " ^ Int.toString revision ^ ")")
      in
        Option.app MySQL.free res
      end
  in
    app insert players
    ; MySQL.close m
  end
