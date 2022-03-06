--xs1 = [ x^2 |x <- [1..10], x `rem` 3 == 2]
--xs2 = [(x,y)| x<- [1..5], y <- [x..(x+2)]]
--xs3 = [(x,y)| x<-[1..3], let k = x^2, y <- [1..k]]
--xs4 = [ x | x<- "Facultatea de Matematica si Informatica", elem x ['A'..'Z']]
--xs5 = [[x..y]| x <- [1..5], y <- [1..5], x < y]

--1
factori :: Int -> [Int]
factori 1 = [1]
factori n = [x | x <- [1..n], n `rem` x == 0]

--2
prim :: Int -> Bool
prim n = if factori n == [1,n] then True
         else False

--3
numerePrime :: Int -> [Int]
numerePrime n = [ x | x <- [2..n], prim x]

--4
myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : myzip3 xs ys zs

--5
ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = and [ a <= b | (a,b) <- zip xs (tail xs)]
-- ordonataNat (x:xs) = and [ a <= b | (a,b) <- zip (x:xs) xs ]

--6
ordonataNat1 :: [Int] -> Bool
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 xs | head xs > head (tail xs) = False
                | otherwise = (ordonataNat1 (tail xs))

--7
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata xs c = and [ c a b | (a,b) <- zip xs (tail xs)]

divizibil x y = if x `mod` y == 0 then True else False

--8
(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(a,b) *<* (c,d) = (a+b) <= (c+d)

infixr 6 *<*

--9
compuneList :: (b -> c) -> [(a -> b)] -> [(a -> c)]
compuneList g lf = [ g . f | f <- lf ]

--10
aplicaList :: a -> [(a->b)] -> [b]
aplicaList x lf = [ f x | f <- lf ]

aplicaList2 x [] = [] 
aplicaList2 x (f:lf) = f x : aplicaList2 x lf 

aplicaList3 x lf = map ($ x) lf