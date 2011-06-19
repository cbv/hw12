structure Timecube =
struct
  open Kompiler

  val f = Card Card.Put
  val t = Card Card.K
  val s = Card Card.S
  val sc = Card Card.Succ
  val i = Card Card.I

  fun comp x y = Apply(Apply(s, Apply(t, x)), y)

  (* \x -> x + {1,2} respectively *)
  val a = Apply (s, Apply(t, sc))
  val b = Apply (s, Apply(t, comp sc sc))

  val basefour =
      Lambda("x",
             Apply((* x f i *)
                   Apply(Apply(Var "x",f),i),
                   (* x (t a) b i *)
                   Apply(Apply(Apply(Var "x",Apply(t,a)),b),i)
                  )
            )

  fun four v = Apply(basefour,Var v)
  (* fun four v = Apply(Apply(Card Card.Get, Card Card.Zero), Var v) *)
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
