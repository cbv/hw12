(* A concrete dominator structure just has a way
   of constructing a dominator. *)
signature DOMINATOR =
sig

  val create : unit -> DOS.dominator

end
