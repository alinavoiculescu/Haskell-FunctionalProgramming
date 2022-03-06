import Data.List
import Data.Char

--main = putStrLn "Hello, world!"

--1
nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (c:s) = (if palindrom c then nrV c else 0) + nrVocale s
       where
          palindrom n = n == reverse n
          vocale = "aeiouAEIOU"
          nrV "" = 0
          nrV (c:sir)
                | c `elem` vocale = 1 + nrV sir
                | otherwise = nrV sir

ePalindrom :: String -> Bool
ePalindrom str =  str == reverse str 

countVocale :: String -> Int
countVocale str 
    | length str == 0 = 0
    | (head str) `elem` vocals = 1 + countVocale (tail str)
    | otherwise = countVocale (tail str)
    where vocals = "aeiouAEIOU"

nrVocale1:: [String] -> Int
nrVocale1 lista 
    | length lista == 0 = 0
    | ePalindrom (head lista) = countVocale (head lista) + nrVocale1 (tail lista)
    | otherwise = nrVocale1 (tail lista)

countVocaleStr :: String -> Int
countVocaleStr [] = 0
countVocaleStr x
    |(elem (head x) "AaEeIiOoUu") = 1 + countVocaleStr(tail x)
    |otherwise = countVocaleStr(tail x)

nrVocale2 :: [String] -> Int
nrVocale2 [] = 0
nrVocale2 xs
    |(head xs == reverse (head xs)) = countVocaleStr(head xs) + nrVocale2(tail xs)
    |otherwise = nrVocale2(tail xs)

--2
f :: Int -> [Int] -> [Int]
f x [] = []
f x (c:s) = if c `mod` 2 == 0 then c:x: f x s
            else c:f x s


semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]


--3
divizori :: Int -> [Int]
divizori 1 = [1]
divizori x = [ y | y <- [1..x], x `mod` y == 0]

--4
listadiv :: [Int] -> [[Int]]
listadiv xs = [ divizori x | x <- xs ]

--5
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec x y [] = []
inIntervalRec x y (n:xs) = if x <= n && n <= y then n : inIntervalRec x y xs
                        else inIntervalRec x y xs

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp x y xs = [ n | n <- xs, x <= n && n <= y ]

--6
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs) = (if x > 0 then nrPoz + 1 else 0) + pozitiveRec xs
        where nrPoz = 0

pozitiveComp :: [Int] -> Int
pozitiveComp xs = length [ x | x <- xs, x > 0 ]

--Nu putem folosi doar descrieri de liste pentru ca acestea ne returneaza tot liste,
--iar noi avem nevoie de un numar, nu de o lista

--7
pozitiiImpareRecA :: [Int] -> Int -> [Int]
pozitiiImpareRecA [] p = []
pozitiiImpareRecA (x:xs) p = if x `mod` 2 == 1 then p : pozitiiImpareRecA xs (p+1)
                          else pozitiiImpareRecA xs (p+1)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec xs = pozitiiImpareRecA xs 0

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp xs = [ i | (i,x) <- [0..] `zip` xs, odd x]

--8
multDigitsRec :: [Char] -> Int
multDigitsRec "" = 1
multDigitsRec (c:sir) = (if isDigit(c) then p * digitToInt(c) else 1) * multDigitsRec sir
              where p = 1

multDigitsComp :: [Char] -> Int
multDigitsComp sir = product [ digitToInt(c) | c <- sir, isDigit(c) ]