(* More useful Combinators for building strategies (aka Dominators). *)
structure Combos =
struct

  open LTG

  (* Returns a sequence of turns that loads the value of integer n
  into slot j *)
  fun loadn n j = 
    let 
      fun load 0 acc = (RightApply (j, Zero)) :: acc
        | load 1 acc = (LeftApply (Succ, j)) :: load 0 acc
	| load n acc = if n mod 2 = 0 then
	                 (LeftApply (Dbl, j)) :: (load (n div 2) acc)
		       else (LeftApply (Succ, j)) :: (load (n - 1) acc)
    in
      rev (load n [LeftApply (Put, j)])
    end
  
  (* Other combinators that might be useful:
      - call a function in slot without destroying it or yourself
   *)



  (* Sketchier from here on... *)


  (* It seems like we might want to write code without the crazy
    left-right application.  See the examples in the problem statement
    for an idea of how to add S's and K's to get around this.

    The code below translates from a general combinator language, e.g.
          (A B) C (D E)
    To one with only left and right applications, e.g.
          ((S(K((((S(K(S(K(S(KI))))))A)B)C)))D)E
    (spoons: I think. I haven't double checked that one.)
    TODO: make this useful
  *)

  (* Arbitrary combinator term *)
  datatype tree = Card of string
                | App of tree * tree

  (* Combinator terms with only left and right applications of
  constants. *)
  datatype leftright = Leaf of string
                     | L of string * leftright
		     | R of leftright * string

  (* Converts from one to the other *)
  fun T (y, Card c) = R (y, c)
    | T (y, App (Card c, t)) 
      (* S(Ky) c t *)
    = T (R (L ("S", L("K", y)), c), t)
    | T (y, App (App (t, u), v))
      (* S(K(S(Ky))) t u v *)
    = T (T (T (L ("S", L ("K", L ("S", L ("K", y)))), t), u), v)

end
