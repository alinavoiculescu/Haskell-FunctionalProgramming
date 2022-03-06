import Data.List (nub)
import Data.Maybe (fromJust)

type Nume = String

data Prop = Var Nume | F | T | Not Prop | Prop :|: Prop | Prop :&: Prop | Prop :->: Prop | Prop :<->: Prop
        deriving (Eq, Read)

infixr 2 :|:
infixr 3 :&:

--1
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

--2
instance Show Prop where
    show (Var x) = x
    show (x :|: y) = "(" ++ show x ++ "|" ++ show y ++ ")"
    show (x :&: y) = "(" ++ show x ++ "&" ++ show y ++ ")"
    show (Not x) = "(~" ++ show x ++ ")"
    show F = "F"
    show T = "T"
    show (x :->: y) = "(" ++ show x ++ "->" ++ show y ++ ")"
    show (x :<->: y) = "(" ++ show x ++ "<->" ++ show y ++ ")"

test_ShowProp :: Bool
test_ShowProp = show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

-------------------------
type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a
-------------------------

--3
eval :: Prop -> Env -> Bool
eval (Var x) n = impureLookup x n
eval F n = False
eval T n = True
eval (Not p) n = not (eval p n)
eval (p :|: q) n = eval p n || eval q n
eval (p :&: q) n = eval p n && eval q n
eval (p :->: q) n = eval p n <= eval q n
eval (p :<->: q) n = (eval p n <= eval q n) && (eval q n <= eval p n)

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

--4
variabile :: Prop -> [Nume]
variabile x = nub (variabile1 x)

variabile1 :: Prop -> [Nume]
variabile1 (Var x) = [x]
variabile1 F = []
variabile1 T = []
variabile1 (Not p) = variabile1 p
variabile1 (p :|: q) = variabile1 p ++ variabile1 q
variabile1 (p :&: q) = variabile1 p ++ variabile1 q
variabile1 (p :->: q) = variabile1 p ++ variabile1 q
variabile1 (p :<->: q) = variabile1 p ++ variabile1 q

test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

--5
envs :: [Nume] -> [Env]
envs [] = [[]]
envs (x:xs) = [ (x, True) : e | e <- envs xs] ++ [ (x, False) : e | e <- envs xs]
 
test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

--6
satisfiabila :: Prop -> Bool
satisfiabila p = or [eval p x | x <- envs (variabile p) ]

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

--7
valida :: Prop -> Bool
valida p = and [eval p x | x <- envs (variabile p) ]

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

--8 (9 in pdf)
--am modificat

--9 (10 in pdf)
--echivalenta :: Prop -> Prop -> Bool
echivalenta p q = valida (p :<->: q)
 
test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))
