----I.

--1
--a)

--2
--c)

--3
--a)

--4
--a)

--5
--a)

--6
--a)

--7
--a)

--8
--a)

--9
--a)

--10
--a)


----II.

--1
prefixComun :: String -> String -> String
prefixComun _ [] = []
prefixComun [] _ = []
prefixComun (x:xs) (y:ys) = if x == y then x : prefixComun xs ys
                            else prefixComun xs ys

--2
sumProd :: [Int] -> [Int] -> Int
sumProd xs ys = sum (sumProdhelper xs ys)

sumProdhelper :: [Int] -> [Int] -> [Int]
sumProdhelper [] [] = []
sumProdhelper _ [] = error "lungimi diferite"
sumProdhelper [] _ = error "lungimi diferite"
sumProdhelper (x:xs) (y:ys) = x^2 * y^2 : sumProdhelper xs ys


----III.
data PairInt = P Int Int deriving Show
data MyList = L [PairInt] deriving Show

data Exp = I Int | Add Exp Exp | Mul Exp Exp deriving Show

class MyClass m where
       toExp :: m -> Exp

--a)
instance MyClass MyList where
      toExp (L []) = I 1
      toExp (L ( (P x y) : xs )) = Mul (Add (I x) (I y)) (toExp (L xs))

--b)
eval :: MyList -> Int
eval list = evalE (toExp list)

evalE :: Exp -> Int
evalE (I x) = x
evalE (Add x y) = evalE x + evalE y
evalE (Mul x y) = evalE x * evalE y