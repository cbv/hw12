structure CardHelp =
struct
 val Card = Kompiler.Card
 infix 9 --
 val op -- = Kompiler.Apply
 val i = Card Card.I
 val zero = Card Card.Zero
 fun succ x = Card Card.Succ -- x
 fun dbl x = Card Card.Dbl -- x
 fun get x = Card Card.Get -- x
 fun put x y = Card Card.Put -- x -- y
 fun s x y z = Card Card.S -- x -- y -- z
 fun k x y = Card Card.K -- x -- y
 fun inc x = Card Card.Inc -- x
 fun dec x = Card Card.Dec -- x
 fun attack x y z = Card Card.Attack -- x -- y -- z
 fun help x y z = Card Card.Help -- x -- y -- z
 fun copy x = Card Card.Copy -- x
 fun revive x = Card Card.Revive -- x
 fun zombie x y = Card Card.Zombie -- x -- y
end
