import Prelude hiding (succ)

data Exp = Zero
         | Succ Exp
         | Dbl Exp
         | Var String
         | Lam String Exp
         | App Exp Exp

shownum :: Exp -> Maybe Int
shownum Zero = Just 0
shownum (Succ e) = fmap (+1) $ shownum e
shownum (Dbl e) = fmap (*2) $ shownum e
shownum _ = Nothing

instance Show Exp where
    show e = case shownum e of Just n -> show n; Nothing -> show' e
        where show' (Zero) = "0"
              show' (Succ e) =
                  let count :: Exp -> (Maybe Exp, Int)
                      count (Succ e) =
                          let (b,n) = count e in (b, n+1)
                      count (Zero) = (Nothing, 0)
                      count e = (Just e, 0)
                      (b, n) = count (Succ e)
                  in show n ++ case b of Nothing -> ""; Just e -> " + " ++ show e
              show' (Dbl e) =
                  let count :: Exp -> (Maybe Exp, Int)
                      count (Dbl e) =
                          let (b,n) = count e in (b, n*2)
                      count (Zero) = (Nothing, 0)
                      count e = (Just e, 1)
                      (b, n) = count (Dbl e)
                  in show n ++ case b of Nothing -> ""; Just e -> " * " ++ show e
              show' (Var s) = s
              show' (Lam s e) = "(\\" ++ s ++ "." ++ show e ++ ")"
              show' (App e1 (App e2 e3)) = show e1 ++ " (" ++ show e2 ++ " " ++ show e3 ++ ")"
              show' (App e1 e2) = show e1 ++ " " ++ show e2

-- left-associative infix app
a $$ b = App a b

-- [e1/x]e2
subst :: Exp -> String -> Exp -> Exp
subst e1 x Zero = Zero
subst e1 x (Succ e) = Succ $ subst e1 x e
subst e1 x (Dbl e) = Dbl $ subst e1 x e
subst e1 x e2@(Var y) = if x == y then e1 else e2
subst e1 x e2@(Lam y e3) = if x == y then e2 else Lam y $ subst e1 x e3
subst e1 x e2@(App e3 e4) = App (subst e1 x e3) (subst e1 x e4)

-- eval
eval :: Exp -> Exp
eval (Succ e) = Succ $ eval e
eval (Dbl e) = Dbl $ eval e
eval (App e1 e2) =
    case eval e1 of
        Lam x e1' -> eval $ subst (eval e2) x e1'
        e1' -> error $ "tried to eval " ++ show e1
eval e = e

-- basic combinators/etc

s = Lam "a" $ Lam "b" $ Lam "c" $ Var "a" $$ Var "c" $$ (Var "b" $$ Var "c")

i = Lam "x" $ Var "x"

t = Lam "x" $ Lam "y" $ Var "x"

f = Lam "x" $ Lam "y" $ Var "y"

-- Interesting combinators below

-- maps [T, I, F, S] to lambdas: \x -> x + [0, 1, 2, 3]
basefour =
    let -- \fx -> succ(fx)
        a = Lam "f" $ Lam "x" $ Succ $ Var "f" $$ Var "x"
        -- \fx -> succ(succ(fx))
        b = Lam "f" $ Lam "x" $ Succ $ Succ $ Var "f" $$ Var "x"
    in Lam "x" $ Var "x" $$ f $$ i $$ (Var "x" $$ (t $$ a) $$ b $$ i)

-- \x y z w -> get 0 w $ quad $ get 0 z $ quad $ get 0 y $ quad $ get 0 x
gen256 =
    let quad = Dbl . Dbl
    in Lam "x" $ Lam "y" $ Lam "z" $ Lam "w" $
        App (basefour $$ Var "x") $ quad $
            App (basefour $$ Var "y") $ quad $
                App (basefour $$ Var "z") $ quad $
                    App (basefour $$ Var "w") Zero

nums = [
        [t, t, t, t], [i, t, t, t], [f, t, t, t], [s, t, t, t], -- 0-15
        [t, i, t, t], [i, i, t, t], [f, i, t, t], [s, i, t, t],
        [t, f, t, t], [i, f, t, t], [f, f, t, t], [s, f, t, t],
        [t, s, t, t], [i, s, t, t], [f, s, t, t], [s, s, t, t],

        [t, t, i, t], [i, t, i, t], [f, t, i, t], [s, t, i, t], -- 16-31
        [t, i, i, t], [i, i, i, t], [f, i, i, t], [s, i, i, t],
        [t, f, i, t], [i, f, i, t], [f, f, i, t], [s, f, i, t],
        [t, s, i, t], [i, s, i, t], [f, s, i, t], [s, s, i, t],

        [t, t, f, t], [i, t, f, t], [f, t, f, t], [s, t, f, t], -- 32-47
        [t, i, f, t], [i, i, f, t], [f, i, f, t], [s, i, f, t],
        [t, f, f, t], [i, f, f, t], [f, f, f, t], [s, f, f, t],
        [t, s, f, t], [i, s, f, t], [f, s, f, t], [s, s, f, t],

        [t, t, s, t], [i, t, s, t], [f, t, s, t], [s, t, s, t], -- 48-63
        [t, i, s, t], [i, i, s, t], [f, i, s, t], [s, i, s, t],
        [t, f, s, t], [i, f, s, t], [f, f, s, t], [s, f, s, t],
        [t, s, s, t], [i, s, s, t], [f, s, s, t], [s, s, s, t],

        [t, t, t, i], [i, t, t, i], [f, t, t, i], [s, t, t, i], -- 64-127
        [t, i, t, i], [i, i, t, i], [f, i, t, i], [s, i, t, i],
        [t, f, t, i], [i, f, t, i], [f, f, t, i], [s, f, t, i],
        [t, s, t, i], [i, s, t, i], [f, s, t, i], [s, s, t, i],

        [t, t, i, i], [i, t, i, i], [f, t, i, i], [s, t, i, i],
        [t, i, i, i], [i, i, i, i], [f, i, i, i], [s, i, i, i],
        [t, f, i, i], [i, f, i, i], [f, f, i, i], [s, f, i, i],
        [t, s, i, i], [i, s, i, i], [f, s, i, i], [s, s, i, i],

        [t, t, f, i], [i, t, f, i], [f, t, f, i], [s, t, f, i],
        [t, i, f, i], [i, i, f, i], [f, i, f, i], [s, i, f, i],
        [t, f, f, i], [i, f, f, i], [f, f, f, i], [s, f, f, i],
        [t, s, f, i], [i, s, f, i], [f, s, f, i], [s, s, f, i],

        [t, t, s, i], [i, t, s, i], [f, t, s, i], [s, t, s, i],
        [t, i, s, i], [i, i, s, i], [f, i, s, i], [s, i, s, i],
        [t, f, s, i], [i, f, s, i], [f, f, s, i], [s, f, s, i],
        [t, s, s, i], [i, s, s, i], [f, s, s, i], [s, s, s, i]
       ]

nums' = map (\[x,y,z,w] -> eval $ gen256 $$ x $$ y $$ z $$ w) nums
