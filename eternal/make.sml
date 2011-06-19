(* Given a base string, builds the appropriate executables *)

structure Make:> sig
   (* Takes a base, like "hopeless" or "hopeless:102" and returns
    * a path to player-hopeless.exe or player-hopeless-102.exe *)
   val getExe: string -> string
   exception MakeFailed
end = struct

val flagSubmit = Params.flag false 
   (SOME ("--submit", "ONLY USE IF YOU'VE TALKED TO ROB")) "submit"

datatype player_file = CUR of string | REV of string * int

exception BadPlayer

exception MakeFailed

fun parse_player_arg s =
   case String.tokens (fn x => x = #":") s of
      [] => raise BadPlayer
    | [name] => CUR name
    | [name, num] => (case Int.fromString num of NONE => raise BadPlayer
                                               | SOME n => REV (name, n))
    | _ => raise BadPlayer

(* build_target and build_filename include messy hack to get around 
 * joshua_'s change to the build process on Friday night *)
fun build_target (CUR name) = "player-" ^ name ^ ".exe"
  | build_target (REV (name, num)) = 
    if num > 175 then "player-" ^ name ^ ".exe"
    else  "player-" ^ name

fun build_filename (x as (CUR name)) = build_target x
  | build_filename (x as (REV (name, num))) = 
    if num > 175 then build_target x
    else "player-" ^ name ^ ".exe"

fun save_filename (CUR name) = "player-" ^ name ^ ".exe"
  | save_filename (REV (name, num)) = 
    "player-" ^ name ^ "-" ^ (Int.toString num) ^ ".exe"

exception Archive
fun archive_filename (CUR name) = raise Archive
  | archive_filename (REV (name, num)) = 
    "player-" ^ name ^ "-" ^ Int.toString num ^ ".tgz"

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

      val _ = 
         if !flagSubmit andalso not(file_exists (archive_filename player))
         then case player of 
            CUR _ => raise Archive
          | REV (name, n) => 
            OS.Process.system("./submission " ^ name ^ " " ^ Int.toString n)
         else OS.Process.success

      val filename = OS.Path.joinDirFile {dir = OS.FileSys.getDir (),
                                          file = save_filename player}
   in
      if file_exists filename then filename
      else raise MakeFailed
   end
end
