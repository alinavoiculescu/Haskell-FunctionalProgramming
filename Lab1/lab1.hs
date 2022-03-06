import Data.List
import Data.Char
--main = putStrLn "Hello, world!"

--1
poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a*x^2+b*x+c

--2
eeny :: Integer -> String
eeny x = if even x then "eeny"
         else "meeny"

--3
fizzbuzz :: Integer -> String
fizzbuzz x = if (x `mod` 3 == 0 && x `mod` 5 == 0) then "FizzBuzz"
         else if (x `mod` 3 == 0) then "Fizz"
         else if (x `mod` 5 == 0) then "Buzz"
         else ""

fizzbuzz' :: Integer -> String
fizzbuzz' x
            | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
            | x `mod` 3 == 0                   = "Fizz"
            | x `mod` 5 == 0                   = "Buzz"
            | otherwise                        = ""

--
fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
                  | n < 2 = n
                  | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)

fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n = fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
--

--4
tribonacciCazuri :: Integer -> Integer
tribonacciCazuri n
                   | n == 1    = 1
                   | n == 2    = 1
                   | n == 3    = 2
                   | otherwise = tribonacciCazuri (n - 1) + tribonacciCazuri (n - 2) + tribonacciCazuri (n - 3)

tribonacciEcuational :: Integer -> Integer
tribonacciEcuational 1 = 1
tribonacciEcuational 2 = 1
tribonacciEcuational 3 = 2
tribonacciEcuational n = tribonacciEcuational (n - 1) + tribonacciEcuational (n - 2) + tribonacciEcuational (n - 3)

--5
binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n - 1) k + binomial (n - 1) (k - 1)

--6a
verifL :: [Int] -> Bool
verifL xs = even (length xs)

--6b
takefinal :: [Int] -> Int -> [Int]
takefinal xs n = if n <= length xs then drop (length xs - n) xs
                 else xs

--6c
remove :: [Int] -> Int -> [Int]
remove xs n = take n xs ++ takefinal xs (length xs - n - 1)

--
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
                 | even h = h `div` 2 : t'
                 | otherwise = t'
            where t' = semiPareRec t
--

--7a
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n v = v : myreplicate (n - 1) v

--7b
sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (i:xs)
              | odd i     = i + xs'
              | otherwise = xs'
        where xs' = sumImp xs

sumImp' :: [Int] -> Int
sumImp' xs 
    | length xs == 0 = 0
    | otherwise = (if (even (head xs)) then 0
                   else head xs) + sumImp' (tail xs)

--7c
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (s:xs)
                | length s > 0 && s !! 0 == 'A' = length s + xs'
                | otherwise                     = xs'
        where xs' = totalLen xs

totalLen' :: [String] -> Int
totalLen' [] = 0
totalLen' (x:xs) = if x /= "" && head x == 'A'
                        then (totalLen' xs) + length x
                   else totalLen' xs