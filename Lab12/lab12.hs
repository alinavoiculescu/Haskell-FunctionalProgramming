{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}

newtype Identity a = Identity a
    deriving Show

instance Functor Identity where
    fmap fct (Identity x) = Identity (fct x)



data Pair a = Pair a a
    deriving Show

instance Functor Pair where
    fmap g (Pair x y) = Pair (g x) (g y)



data Constant a b = Constant b
    deriving Show

instance Functor (Constant a) where
    fmap g (Constant x) = Constant (g x)



data Two a b = Two a b
    deriving Show

instance Functor (Two a) where
    fmap g (Two x y) = Two x (g y)



data Three a b c = Three a b c
    deriving Show

instance Functor (Three a b) where
    fmap g (Three x y z) = Three x y (g z)



data Three' a b = Three' a b b
    deriving Show

instance Functor (Three' a) where
    fmap g (Three' x y z) = Three' x (g y) (g z)



data Four a b c d = Four a b c d
    deriving Show

instance Functor (Four a b c) where
    fmap g (Four x y z t) = Four x y z (g t)



data Four'' a b = Four'' a a a b
    deriving Show

instance Functor (Four'' a) where
    fmap g (Four'' x y z t) = Four'' x y z (g t)



data Quant a b = Finance | Desk a | Bloor b
    deriving Show

instance Functor (Quant a) where
    fmap g Finance = Finance
    fmap g (Desk x) = Desk x
    fmap g (Bloor y) = Bloor (g y)




data LiftItOut f a = LiftItOut (f a)
    deriving Show

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut x) = LiftItOut (fmap f x)



data Parappa f g a = DaWrappa (f a) (g a)
    deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)



data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving Show

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)



data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving Show

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious x y z) = Notorious x y (fmap f z)



data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show

instance Functor GoatLord where
    fmap g NoGoat = NoGoat
    fmap g (OneGoat x) = OneGoat (g x)
    fmap g (MoreGoats x y z) = MoreGoats (fmap g x) (fmap g y) (fmap g z)



data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap g Halt = Halt
    fmap g (Print str x) = Print str (g x)
    fmap g (Read fct) = Read (g . fct)