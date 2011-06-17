import Prelude hiding (succ)

data Exp = Val Int
         | Func (Exp -> Exp)
         | Var String
         | Lam String Exp
         | App Exp Exp

instance Show Exp where
    show (Val i) = show i
    show (Func f) = "<fn>"
    show (Var s) = s
    show (Lam s e) = "(\\" ++ s ++ "." ++ show e ++ ")"
    show (App e1 (App e2 e3)) = show e1 ++ " (" ++ show e2 ++ " " ++ show e3 ++ ")"
    show (App e1 e2) = show e1 ++ " " ++ show e2

-- left-associative infix app
a $$ b = App a b

-- [e1/x]e2
subst :: Exp -> String -> Exp -> Exp
subst e1 x e2@(Val _) = e2
subst e1 x e2@(Func _) = e2
subst e1 x e2@(Var y) = if x == y then e1 else e2
subst e1 x e2@(Lam y e3) = if x == y then e2 else Lam y $ subst e1 x e3
subst e1 x e2@(App e3 e4) = App (subst e1 x e3) (subst e1 x e4)

-- eval
eval :: Exp -> Exp
eval (App e1 e2) =
    case eval e1 of
        Func e1' -> e1' $ eval e2
        Lam x e1' -> subst (eval e2) x e1'
        e1' -> error $ "tried to eval " ++ show e1'
eval e = e

-- basic combinators/etc

s = Lam "a" $ Lam "b" $ Lam "c" $ App (App (Var "a") (Var "c"))
                                      (App (Var "b") (Var "c"))

i = Lam "x" $ Var "x"

t = Lam "x" $ Lam "y" $ Var "x"

f = Lam "x" $ Lam "y" $ Var "y"

succ (Val x) = Val $ x + 1
succ e = error $ "tried to succ " ++ show e

dbl (Val x) = Val $ x * 2
dbl e = error $ "tried to dbl " ++ show e

-- Interesting combinators below

basefour x =
    x $$ f $$ i $$ (x $$ (t $$ Func succ) $$ (Func $ succ . succ) $$ i)
