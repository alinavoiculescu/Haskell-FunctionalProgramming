import Data.Monoid

------------------------------------------------------------------------------------------------------------------------------------------
data BinaryTree a =
  Empty
  | Leaf a
  | Node ( BinaryTree a ) ( BinaryTree a )
    deriving Show

foldTree :: ( a -> b -> b ) -> b -> BinaryTree a -> b
foldTree f i Empty = i
foldTree f i ( Leaf x ) = f x i
foldTree f i (Node l r ) = foldTree f ( foldTree f i r ) l

myTree = Node (Node ( Leaf 1) ( Leaf 2) ) (Node ( Leaf 3) (Leaf 4) )

instance Foldable BinaryTree where
  foldr = foldTree

myTreeSum = Node (Node ( Leaf (Sum 1)) ( Leaf (Sum 2)) ) (Node ( Leaf (Sum 3)) (Leaf (Sum 4)) )
myTreeProduct = Node (Node ( Leaf (Product 1)) ( Leaf (Product 2)) ) (Node ( Leaf (Product 3)) (Leaf (Product 4)) )
------------------------------------------------------------------------------------------------------------------------------------------

--1
elemList1 :: (Eq a) => a -> [a] -> Bool
elemList1 x xs = foldr (||) False (map (\a -> a == x) xs)

elemList2 :: (Eq a) => a -> [a] -> Bool
elemList2 x xs = foldr (\y acc -> acc || x == y) False xs

elemFoldr :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldr x xs = foldr (\y acc -> acc || x == y) False xs

elemFoldMap :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldMap x xs = getAny $ foldMap ( \y -> Any (y == x) ) xs


nullFoldr :: (Foldable t) => t a -> Bool
nullFoldr xs = foldr (\x acc -> acc && False) True xs 

nullFoldMap :: (Foldable t) => t a -> Bool
nullFoldMap xs = getAll $ foldMap ( \x -> All False) xs 


lengthList1 :: [a] -> Int
lengthList1 [] = 0
lengthList1 (x:xs) = 1 + lengthList1 xs

lengthList2 :: [a] -> Int
lengthList2 xs = foldr (\x acc -> acc + 1) 0 xs

lengthFoldr :: Foldable t => t a -> Int
lengthFoldr xs = foldr (\x acc -> acc + 1) 0 xs

lengthFoldMap :: Foldable t => t a -> Int
lengthFoldMap xs = getSum $ foldMap ( \x -> Sum 1 ) xs


toListFoldr :: (Foldable t) => t a -> [a]
toListFoldr xs = foldr (\x acc -> x : acc) [] xs

toListFoldMap :: (Foldable t) => t a -> [a]
--toListFoldMap xs = foldMap ( \x -> x : [] ) xs
toListFoldMap xs = foldMap ( \x -> [x] ) xs

fold :: (Foldable t, Monoid m) => t m -> m
fold xs = foldMap id xs


--2
data Constant a b = Constant b

instance Foldable (Constant a) where
     foldMap f (Constant x) = f x


data Two a b = Two a b

instance Foldable (Two a) where
     foldMap f (Two x y) = f y


data Three a b c = Three a b c

instance Foldable (Three a b) where
     foldMap f (Three x y z) = f z


data Three' a b = Three' a b b

instance Foldable (Three' a) where
     foldMap f (Three' x y z) = f y <> f z


data Four' a b = Four' a b b b

instance Foldable (Four' a) where
     foldMap f (Four' x y z t) = f y <> f z <> f t


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Foldable GoatLord where
     foldMap f NoGoat = mempty
     foldMap f (OneGoat x) = f x
     foldMap f (MoreGoats x y z) = (foldMap f x) <> (foldMap f y) <> (foldMap f z)

--ex:
goatlord1 = NoGoat
goatlord2 = OneGoat 5
goatlord3 = MoreGoats (OneGoat 5) NoGoat (MoreGoats (OneGoat 1) (OneGoat 6) NoGoat)