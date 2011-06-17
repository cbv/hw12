structure Timecube =
struct
  open Kompiler
  
  (* \x -> x + {1,2} respectively *)
  val a = Lambda ("f", Lambda ("x", Apply (Card Card.Succ, Apply (Var "f", Var "x"))));
  val b = Lambda ("f", Lambda ("x", Apply (Card Card.Succ, Apply (Card Card.Succ, Apply (Var "f", Var "x")))));
  
  val f = Card Card.Put
  val t = Card Card.K
  val s = Card Card.S
  val i = Card Card.I

  val basefour =
      Lambda("x",
             Apply((* x f i *)
                   Apply(Apply(Var "x",f),i),
                   (* x (t a) b i *)
                   Apply(Apply(Var "x",Apply(t,b)),i)
                  )
            )

  (* fun four v = Apply(basefour,Var v) *)
  fun four v = Apply(Apply(Card Card.Get, Card Card.Zero), Var v)
  fun quad x = Apply(Card Card.Dbl,(Apply(Card Card.Dbl,x)))

  val timecube =
      Lambda("x",Lambda("y",Lambda("z",Lambda("w",
        (* \xyzw -> *)
        Apply(four "x",
              quad(Apply(four "y",
                         quad(Apply(four "z",
                                    quad(Apply(four "w", Card Card.Zero))
                                    ))
                        ))
             )
      ))))
end
