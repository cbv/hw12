(* This shared services generates cells with numbers. *)
signature NUMBER_GENERATOR =
sig
  (* For starting the number generator background process. *)
  include DOMINATOR

  datatype status
    = NotDone
    | Done of int (* the number of the slot with required number *)

  (* Create a slot with the given number in it.  Returns a reference to the
     status.  When Done, the caller owns the given slot.  Note that it may
     return Done immediately, so you don't necessarily need to wait a whole
     turn.

     You should release that slot before requesting additional numbers since
     the generator will reclaim unallocated slots with useful values.
  *)
  val generate : DOS.dos -> int -> status ref

end
