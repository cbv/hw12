(* Given a base string, builds the appropriate executables *)

structure Make:> sig
   (* Takes a base, like "hopeless" or "hopeless:102" and returns
    * a path to player-hopeless.exe or player-hopeless-102.exe *)
   val getExe: string -> string
end = struct

datatype player_file = CUR of string | REV of string * int

exception BadPlayer

fun parse_player_arg s =
   case String.tokens (fn x => x = #":") s of
      [] => raise BadPlayer
    | [name] => CUR name
    | [name, num] => (case Int.fromString num of NONE => raise BadPlayer
                                               | SOME n => REV (name, n))
    | _ => raise BadPlayer

fun build_target (CUR name) = "player-" ^ name
  | build_target (REV (name, num)) = "player-" ^ name

fun build_filename x = (build_target x) ^ ".exe"

fun save_filename (CUR name) = "player-" ^ name ^ ".exe"
  | save_filename (REV (name, num)) = 
    "player-" ^ name ^ "-" ^ (Int.toString num) ^ ".exe"

fun file_exists file = 
   case (SOME (OS.FileSys.fullPath file) handle SysErr => NONE) of
      SOME _ => true
    | NONE => false

fun getExe base = 
   let 
      val player = parse_player_arg(base) 
      val _ = if (file_exists (save_filename player)) then () else
         case player of
            REV (_, n) => (OS.Process.system("svn up -r " ^ (Int.toString n));
                           OS.Process.system("make " ^ (build_target player));
                           OS.Process.system("mv " ^ (build_filename player)
                                             ^ " " ^ (save_filename player));
                           OS.Process.system("svn up");
                           ())
          | CUR _ => (OS.Process.system("make " ^ (build_target player));
                      ())
      val filename = OS.Path.joinDirFile {dir = OS.FileSys.getDir (),
                                          file = save_filename player}
   in
      filename
   end
end
