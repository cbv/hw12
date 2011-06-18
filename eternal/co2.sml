val line = fn () => TextIO.inputLine TextIO.stdIn
val err = fn s => (TextIO.output (TextIO.stdErr, "Error: " ^ s ^ "\n")
                   ; OS.Process.exit OS.Process.failure)

val current_players = 
  let
    fun loop players = 
      let val newline = Option.map (String.tokens Char.isSpace) (line ()) in
        case newline of
          NONE => players
        | SOME [] => loop players (* ignore whitespace *)
        | SOME [ name, number ] => 
          (case (String.tokens Char.isAlphaNum name, Int.fromString number) of
             ([], SOME number) => loop ((name, number) :: players)
           | _ => err ("Line \"" ^ name ^ " " ^ number ^ "\" bad"))
        | SOME ss => err ("Line \"" ^ String.concatWith " " ss ^ "\" bad")
      end
  in
    loop []
  end

val m = MySQL.connect "root" (SOME "R_E_D_A_C_T_E_D")

val _ = MySQL.query m ("use hw12")

val stored_players = 
  let
    val res = valOf (MySQL.query m ("select * from contestants"))
      handle MySQL.MySQL s => err s
           | Option => err "Nothing in query, weird..." 

    fun loop players = 
      case MySQL.readone m res of
        NONE => players
      | SOME [ SOME (MySQL.Int id), SOME (MySQL.String name),
               SOME (MySQL.Int rev), SOME (MySQL.Int active) ] =>
        loop ((id, name, rev, active > 0) :: players)
      | _ => err "unexpected database response" 
  in
    loop [] before MySQL.free res
  end

fun checknew (name, rev) = 
  let fun mapper (id, name', rev', active) = 
        if name = name' andalso rev = rev'
        then SOME (id, active) else NONE
  in 
    case List.mapPartial mapper stored_players of  
      [] =>
      let
        val query = "insert into contestants values (NULL, "
        val query = query ^ "\"" ^ name ^ "\", "
        val query = query ^ Int.toString rev ^ ", 1)"
        val res = MySQL.query m query
          handle MySQL.MySQL s => err s
      in
        Option.app MySQL.free res
      end
    | [ (_, true) ] => ()
    | [ (id, false) ] => 
      let 
        val query = "update contestants set active = 1 where id = "
        val res = MySQL.query m (query ^ Int.toString id)
          handle MySQL.MySQL s => err s
      in 
        Option.app MySQL.free res
      end
    | _ => err ("duplicate: " ^ name ^ ":" ^ Int.toString rev)
  end

val () = app checknew current_players

fun checkold (id, name, rev, active) =
  let fun mapper (name', rev') = name = name' andalso rev = rev' 
  in
    if not active orelse List.exists mapper current_players then ()
    else let (* active is true and name:rev not in current players *)
        val query = "update contestants set active = 0 where id = "
        val res = MySQL.query m (query ^ Int.toString id)
          handle MySQL.MySQL s => err s
      in
        Option.app MySQL.free res 
      end
  end    

val () = app checkold stored_players

val () = MySQL.close m

