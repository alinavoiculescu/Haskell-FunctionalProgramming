import Data.Char
import Data.List

--1
rotate :: Int -> [Char] -> [Char]
rotate n xs = if (n < 0 || n > length xs) then error "n negativ sau prea mare"
              else drop n xs ++ take n xs

--2
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l
----Functia verifica daca rotirea de k litere a rotirii de k litere este
----egala cu sirul nostru
----Eroarea este evitata deoarece "k `mod` l" va fi mereu < l

--3
makeKey :: Int -> [(Char, Char)]
makeKey n = [(x,y) | (x,y) <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `zip` rotate n "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]

--4
lookUp :: Char -> [(Char, Char)] -> Char
lookUp x [] = x
lookUp x (y:xs) = if x == fst y then snd y
                  else lookUp x xs

--5
encipher :: Int -> Char -> Char
encipher x c = lookUp c (makeKey x)

--6
normalize :: String -> String
normalize xs = [ toUpper x | x <- xs, isAlphaNum x ]

--7
encipherStr :: Int -> String -> String
encipherStr x xs = [ encipher x a | a <- normalize xs ]

--8
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [ (y,x) | (x,y) <- xs ]

--9
decipher :: Int -> Char -> Char
decipher x c = lookUp c (reverseKey (makeKey x))

decipherStr :: Int -> String -> String
decipherStr x xs = [ decipher x a | a <- xs, isDigit a || isUpper a || isSpace a ]