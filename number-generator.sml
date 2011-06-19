structure NumberGenerator :> NUMBER_GENERATOR = (* XXX DOMINATOR *)
struct
structure GS = GameState

exception NumberGenerator of string

datatype status
  = NotDone
  | Done of int

(* States:
    - no slot: get a slot
    - got slot: run turns
    - no turns: set status to done
*)
type work = {caller_dos : DOS.dos,
             goal : int,
             slot : int option ref, 
             turns : LTG.turn list ref, 
             status : status ref}
val queue = ref [] : work list ref

(* Keep track of whether or not someone has started us. *)
datatype global_status = NotRunning | Running 
val global_status = ref NotRunning

(* Hard code some useful values?
 val known_valuable_numbers = [31, 63, 127, 8192]
 *)

fun create () =
    let
      fun preview _ = global_status := Running

(* Kompiler.compile (Kompiler.Int goal) *)                      
      fun taketurn dos =
          let
          in
            DOS.Can'tRun
          end
    in
      {preview = preview, taketurn = taketurn}
    end

fun generate caller_dos goal =
    if (!global_status) <> Running then 
      raise NumberGenerator "You must start the number generator service."
    else
      let 
        val gs = DOS.gamestate caller_dos
        val (values, _) = GS.myside gs
        val (_, vitality) = GS.myside gs

        val status = ref NotDone

        (* Used to exit loops below.*)
        exception Found of int

        (* Let's figure out how we are going to do this.*)
        (* 1. Look for existing cells with the right value. *)
        val () = Util.for 0 255 
                 (fn i => if Array.sub (values, i) = LTG.VInt goal 
                             andalso Array.sub (vitality, i) >= 0
                             (* XXX maybe we already own it? *)
                             andalso DOS.reserve_fixed_slot caller_dos i
                          then 
                            raise Found i
                          else ())
            handle Found i => 
                   (status := Done i)


        (* Other strategies ....
            Timecube,
            Precomputed ones,
            Small variations of existing slots,
            etc.
         *)

        (* n. Run the code to build the number in a new cell. *)
        val () = if !status = NotDone then () else
                 (queue := ({goal = goal,
                             caller_dos = caller_dos,
                             slot = ref (DOS.reserve_addressable_slot caller_dos),
                             turns = ref nil,
                             status = status} :: !queue))
      in
        status
      end

end
