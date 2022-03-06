aplica2 :: (a -> a) -> a -> a
aplica2 f x = f (f x)

aplica2' :: (a -> a) -> a -> a
aplica2' f = f . f

aplica2'' :: (a -> a) -> a -> a
aplica2'' = \f x -> f (f x)

aplica2''' :: (a -> a) -> a -> a
aplica2''' f = \x -> f (f x)

--1
firstEl :: [(a,b)] -> [a]
firstEl xs = map fst xs

--2
sumList  :: [[Int]] -> [Int]
--sumList (x:xs) = sum x : sumList xs
sumList xs = map sum xs

--3
prel2 :: [Int] -> [Int]
--prel2 xs = foldr (++) [] [(map (`div` 2) $ filter even xs), (map (*2) $ filter odd xs)]
--prel2 [] = []
--prel2 (x:xs) = if even x then (x `div` 2) : prel2 xs
--               else (x * 2) : prel2 xs

prel2 xs = map (\ x -> if even x then x `div` 2 else x * 2) xs

--4
--f :: Char -> [String] -> [String]
f x xs = filter (\ a -> x `elem` a) xs
--filter (x `elem`) xs

--5
func1 :: [Int] -> [Int]
func1 = map (^2) . filter odd
--func1 xs = map (^2) . filter odd $ xs

--6
patrPozImp :: [Int] -> [Int]
patrPozImp xs = map (\(a,b) -> a^2) (filter (\(a,b) -> mod b 2 == 1) (zip xs [0..]))

--7
numaiVocale :: [String] -> [String]
numaiVocale xs = map (filter (\a -> a `elem` "aeiouAEIOU")) xs

--8
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter p (x:xs) = if p x then x : myfilter p xs
                    else myfilter p xs

--9
sumPatImp :: [Int] -> Int
sumPatImp xs = foldr (+) 0 . map (^2) . filter odd $ xs

--10
checkIfTrue :: [Bool] -> Bool
checkIfTrue xs = foldr (==) True xs

--11
----a
rmChar :: Char -> String -> String
--rmChar c "" = ""
--rmChar c (x:xs) = if x == c then rmChar c xs
--                  else x : rmChar c xs
rmChar c xs = filter (/= c) xs 

----b
rmCharsRec :: String -> String -> String
rmCharsRec [] ys = ys
rmCharsRec (x:xs) ys = rmCharsRec xs (rmChar x ys)

----c
rmCharsFold :: String -> String -> String
--rmCharsFold xs ys = foldr (\x zs -> if (elem x $ xs) then zs else x:zs) "" ys
rmCharsFold xs ys = foldr (rmChar) ys xs