--1
data Fruct
     = Mar String Bool
     | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10

listaFructe = [Mar "Ionatan" False,
               Portocala "Sanguinello" 10,
               Portocala "Valencia" 22,
               Mar "Golden Delicious" True,
               Portocala "Sanguinello" 15,
               Portocala "Moro" 12,
               Portocala "Tarocco" 3,
               Portocala "Moro" 12,
               Portocala "Valencia" 2,
               Mar "Golden Delicious" False,
               Mar "Golden" False,
               Mar "Golden" True]

--a
soiuriS = ["Tarocco", "Moro", "Sanguinello"]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia x = case x of
                                  Portocala s i -> elem s soiuriS
                                  Mar _ _ -> False

ePortocala (Portocala s i) = True
ePortocala (Mar _ _) = False

ePortocalaDeSicilia2 :: Fruct -> Bool
ePortocalaDeSicilia2 (Portocala s _ ) = elem s soiuriS
ePortocalaDeSicilia2 (Mar _ _ ) = False

test_ePortocalaDeSicilia1 = ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 = ePortocalaDeSicilia (Mar "Ionatan" True) == False

--b
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (Portocala s i : lfructe) = if elem s soiuriS then i + nrFeliiSicilia lfructe
                                           else nrFeliiSicilia lfructe
nrFeliiSicilia (Mar _ _ : lfructe) = nrFeliiSicilia lfructe

nrFeliiSicilia2 :: [Fruct] -> Int
nrFeliiSicilia2 lista = sum [ i | Portocala s i <- lista, elem s soiuriS]

nrFeliiSicilia3 :: [Fruct] -> Int
nrFeliiSicilia3 lista = foldr (+) 0 ( map (\ (Portocala s i) -> i ) (filter ePortocalaDeSicilia lista) )

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

--c
nrMereViermi :: [Fruct] -> Int
nrMereViermi lista = sum [ 1 | Mar s i <- filter eMarCuViermi lista]

eMarCuViermi (Mar _ True) = True
eMarCuViermi (Mar _ False) = False
eMarCuViermi (Portocala _ _) = False

test_nrMereViermi = nrMereViermi listaFructe == 2

--2
type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
              deriving Show

--a
vorbeste :: Animal -> String
vorbeste x = case x of
                       Pisica _ -> "Meow!"
                       Caine _ _ -> "Woof!"

--b
rasa :: Animal -> Maybe String
rasa x = case x of
                   Caine _ r -> Just r
                   Pisica _ -> Nothing

--3
data Linie = L [Int]
             deriving Show
data Matrice = M [Linie]
               deriving Show

--a 
verifica :: Matrice -> Int -> Bool
verifica (M xs) n = and (listaverifica (M xs) n)

listaverifica :: Matrice -> Int -> [Bool]
listaverifica (M []) n = []
listaverifica (M ((L x):xs)) n = if (foldr (+) 0 x) == n then True : listaverifica (M xs) n
                                 else False : listaverifica (M xs) n

test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True

--b
doarPozN :: Matrice -> Int -> Bool
doarPozN (M xs) n = and (verificaDoarPozN (M xs) n)

verificaDoarPozN :: Matrice -> Int -> [Bool]
verificaDoarPozN (M []) n = []
verificaDoarPozN (M ((L x):xs)) n = if length x == n then (length (filter (>0) x) == length x) : verificaDoarPozN (M xs) n
                                    else verificaDoarPozN (M xs) n

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True
testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False

--c
corect :: Matrice -> Bool
corect m = and (verificaCorect m)

verificaCorect :: Matrice -> [Bool]
verificaCorect (M []) = [] 
verificaCorect(M [x]) = [] 
verificaCorect (M ((L x) :(L y):xs)) = if length x == length y then True : verificaCorect (M ((L y):xs))
                                      else False : verificaCorect (M ((L y):xs))

--corect (M ((L x):xs)) = all (\ (L a) -> length a == length x) xs

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True