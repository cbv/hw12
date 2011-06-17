import Prelude hiding (succ)

data Exp = Zero
         | Succ Exp
         | Var String
         | Lam String Exp
         | App Exp Exp

instance Show Exp where
    show (Zero) = "0"
    show (Succ e) = "1+" ++ show e
    show (Var s) = s
    show (Lam s e) = "(\\" ++ s ++ "." ++ show e ++ ")"
    show (App e1 (App e2 e3)) = show e1 ++ " (" ++ show e2 ++ " " ++ show e3 ++ ")"
    show (App e1 e2) = show e1 ++ " " ++ show e2

-- left-associative infix app
a $$ b = App a b

-- [e1/x]e2
subst :: Exp -> String -> Exp -> Exp
subst e1 x Zero = Zero
subst e1 x (Succ e) = Succ $ subst e1 x e
subst e1 x e2@(Var y) = if x == y then e1 else e2
subst e1 x e2@(Lam y e3) = if x == y then e2 else Lam y $ subst e1 x e3
subst e1 x e2@(App e3 e4) = App (subst e1 x e3) (subst e1 x e4)

-- eval
eval :: Exp -> Exp
eval (App e1 e2) =
    case eval e1 of
        Lam x e1' -> subst (eval e2) x e1'
        e1' -> error $ "tried to eval " ++ show e1
eval (Succ e) = Succ $ eval e
eval e = e

-- basic combinators/etc

s = Lam "a" $ Lam "b" $ Lam "c" $ App (App (Var "a") (Var "c"))
                                      (App (Var "b") (Var "c"))

i = Lam "x" $ Var "x"

t = Lam "x" $ Lam "y" $ Var "x"

f = Lam "x" $ Lam "y" $ Var "y"

-- Interesting combinators below

basefour x =
    let a = Lam "f" $ Lam "x" $ Succ $ Var "f" $$ Var "x"
        b = Lam "f" $ Lam "x" $ Succ $ Succ $ Var "f" $$ Var "x"
    in x $$ f $$ i $$ (x $$ (t $$ a) $$ b $$ i)
