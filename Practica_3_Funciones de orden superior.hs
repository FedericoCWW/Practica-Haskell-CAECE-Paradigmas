-- ============================================================
-- Práctica PF3 - Funciones de Orden Superior
-- Universidad CAECE - Paradigmas de Programación
-- ============================================================
{- HLINT ignore "Use tuple-section" -}
{- HLINT ignore "Redundant bracket" -}

module PF3 where

-- ============================================================
-- Ejercicio 1 - esCerrada
-- ============================================================

esCerrada :: Eq a => [a] -> (a -> a -> a) -> Bool
esCerrada xs f = and [ f x y `elem` xs | x <- xs, y <- xs ]


-- ============================================================
-- Ejercicio 2 - maxf y minf
-- ============================================================

maxf :: Ord b => (a -> b) -> [a] -> b
maxf f xs = maximum (map f xs)

minf :: (Ord b, Num b) => (a -> b) -> [a] -> b
minf f xs = negate (maxf (negate . f) xs)


-- ============================================================
-- Ejercicio 3 - supf e inff
-- ============================================================

supf :: Ord b => (a -> b) -> (a -> b) -> a -> b
supf f g x = max (f x) (g x)

inff :: Ord b => (a -> b) -> (a -> b) -> a -> b
inff f g x = min (f x) (g x)


-- ============================================================
-- Ejercicio 4 - genLista y rango
-- ============================================================

genLista :: Int -> a -> (a -> a) -> [a]
genLista 0 _ _ = []
genLista n x f = x : genLista (n-1) (f x) f

rango :: Int -> Int -> [Int]
rango n m = genLista (m - n + 1) n (+1)


-- ============================================================
-- Ejercicio 5 - myFilter
-- ============================================================

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
  | p x       = x : myFilter p xs
  | otherwise = myFilter p xs

filtro :: (a -> Bool) -> [a] -> [a]
filtro f [] = []
filtro f (x:xs)= if (f x) then (x: filtro f xs) else (filtro f xs) 


-- ============================================================
-- Ejercicio 6 - Composicion (o), ultimo, segundo
-- ============================================================

o :: (b -> c) -> (a -> b) -> a -> c
o f g x = f (g x)

ultimo :: [a] -> a
ultimo = last

segundo :: [a] -> a
segundo = head . tail


-- ============================================================
-- Ejercicio 7 - curry y uncurry
-- ============================================================

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y


-- ============================================================
-- Ejercicio 8 - Versiones currificadas/no currificadas
-- ============================================================

sumaCurr :: Int -> Int -> Int
sumaCurr x y = x + y

sumaNoCurr :: (Int, Int) -> Int
sumaNoCurr (x, y) = x + y

divCurr :: Int -> Int -> Int
divCurr = div

divNoCurr :: (Int, Int) -> Int
divNoCurr (x, y) = div x y

sucesor :: Int -> Int
sucesor = sumaCurr 1

predecesor :: Int -> Int
predecesor x = sumaCurr x (-1) 

mitad :: Int -> Int
mitad x = divCurr x 2

dosVeces :: (a -> a) -> a -> a
dosVeces f = f . f

cuatroVeces :: (a -> a) -> a -> a
cuatroVeces f = dosVeces (dosVeces f)


-- ============================================================
-- Ejercicio 9 - separar y mayoria
-- ============================================================

separar :: (a -> Bool) -> [a] -> ([a], [a])
separar p xs = (filter p xs, filter (not . p) xs)

mayoriaF :: Int -> [Int] -> Bool
mayoriaF n xs = length (filter (> n) xs) > length (filter (<= n) xs)

mayoria :: Int -> [[Int]] -> [[Int]]
mayoria n = filter (mayoriaF n)


-- ============================================================
-- Ejercicio 10 - paraCada, todos, ninguno, igLong
-- ============================================================

paraCada :: Int -> Int -> b -> (b -> Int -> b) -> b
paraCada ini fin dato f
  | ini > fin = dato
  | otherwise = paraCada (ini+1) fin (f dato ini) f

todos :: [a] -> (a -> Bool) -> Bool
todos xs p = paraCada 0 (length xs - 1) True (\acc i -> acc && p (xs !! i))

ninguno :: [a] -> (a -> Bool) -> Bool
ninguno xs p = todos xs (not . p)

igLong :: [[a]] -> Bool
igLong xss = todos (tail xss) (\xs -> length xs == length (head xss))


-- ============================================================
-- Ejercicio 11 - while, myUntil, y versiones con while
-- ============================================================

while :: a -> (a -> Bool) -> (a -> a) -> a
while x cond f
  | cond x    = while (f x) cond f
  | otherwise = x

myUntil :: a -> (a -> Bool) -> (a -> a) -> a
myUntil x cond f
  | cond (f x) = f x
  | otherwise  = myUntil (f x) cond f

ultimoWhile :: [a] -> a
ultimoWhile xs = head (while xs (\ys -> length ys > 1) tail)

longWhile :: [a] -> Int
longWhile xs = snd $ while (xs, 0) (not . null . fst) (\(ys,n) -> (tail ys, n+1))

sumaListaWhile :: [Int] -> Int
sumaListaWhile xs = snd $ while (xs, 0) (not . null . fst) (\(ys,acc) -> (tail ys, acc + head ys))

-- ============================================================
-- Ejercicio 12 - myMap y mapn
-- ============================================================

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

mapn :: (a -> b, [a]) -> [b]
mapn (_, [])   = []
mapn (f, x:xs) = f x : mapn (f, xs)


-- ============================================================
-- Ejercicio 13 - mapn2 y map2
-- ============================================================

mapn2 :: (a -> b, [[a]]) -> [[b]]
mapn2 (f, xss) = map (map f) xss

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)

-- ============================================================
-- Ejercicio 14 - ArbBin y mapArb
-- ============================================================

data ArbBin a = Hoja | Nodo (ArbBin a) a (ArbBin a) deriving (Show)

mapArb :: (a -> b) -> ArbBin a -> ArbBin b
mapArb _ Hoja = Hoja
mapArb f (Nodo izq x der) = Nodo (mapArb f izq) (f x) (mapArb f der)


-- ============================================================
-- Ejercicio 15 - mapo, mapo2, sumamat
-- ============================================================

mapo :: (a -> b -> c) -> [(a, b)] -> [c]
mapo f = map (uncurry f)

mapo2 :: (a -> b -> c) -> [a] -> [b] -> [c]
mapo2 _ [] _ = []
mapo2 _ _ [] = []
mapo2 f (x:xs) (y:ys) = f x y : mapo2 f xs ys

type Matriz = [[Int]]

sumamat :: Matriz -> Matriz -> Matriz
sumamat = mapo2 (mapo2 (+))


-- ============================================================
-- Ejercicio 16 - simplif
-- ============================================================

simplif :: (b -> c) -> (a -> b) -> [a] -> [c]
simplif f g = map (f . g)


-- ============================================================
-- Ejercicio 17 - sigma
-- ============================================================

sigma :: Int -> Int -> (Int -> Float) -> Float
sigma lower upper termino = foldr ((+) . termino) 0 [lower..upper]


-- ============================================================
-- Ejercicio 18 - pascal
-- ============================================================

paresConsec :: [a] -> [(a, a)]
paresConsec xs = zip xs (tail xs)

siguienteFila :: [Int] -> [Int]
siguienteFila fila = 1 : map (uncurry (+)) (paresConsec fila) ++ [1]

pascal :: Int -> [[Int]]
pascal 0 = [[1]]
pascal n = let prev = pascal (n-1)
           in prev ++ [siguienteFila (last prev)]


-- ============================================================
-- Ejercicio 19 - mapearF y paresEnPosic
-- ============================================================

mapearF :: [a -> b] -> [a] -> [b]
mapearF [] _          = []
mapearF _ []          = []
mapearF (f:fs) (x:xs) = f x : mapearF fs xs

paresEnPosic :: [Int] -> Bool
paresEnPosic xs = and (mapearF (map (\i -> (== 2*i)) [1..]) xs)


-- ============================================================
-- Ejercicio 22 - Recursion de cola
-- ============================================================

factorialTC :: Integer -> Integer
factorialTC n = go n 1
  where go 0 acc = acc
        go k acc = go (k-1) (k * acc)

sumaTC :: [Int] -> Int
sumaTC = go 0
  where go acc []     = acc
        go acc (x:xs) = go (acc + x) xs

revTC :: [a] -> [a]
revTC = go []
  where go acc []     = acc
        go acc (x:xs) = go (x:acc) xs

appendTC :: [a] -> [a] -> [a]
appendTC xs = go (revTC xs)
  where go [] acc     = acc
        go (x:rs) acc = go rs (x:acc)

aparear :: [a] -> [b] -> [(a,b)]
aparear xs ys = go xs ys []
  where go [] _ acc          = revTC acc
        go _ [] acc          = revTC acc
        go (x:rs) (y:ts) acc = go rs ts ((x,y):acc)


-- ============================================================
-- Ejercicio 23 - Recursion mutua
-- ============================================================

myPar :: Int -> Bool
myPar 0 = True
myPar n = myImpar (n - 1)

myImpar :: Int -> Bool
myImpar 0 = False
myImpar n = myPar (n - 1)

cong0 :: Int -> Bool
cong0 0 = True
cong0 n = cong2 (n - 1)

cong1 :: Int -> Bool
cong1 0 = False
cong1 n = cong0 (n - 1)

cong2 :: Int -> Bool
cong2 0 = False
cong2 n = cong1 (n - 1)


-- ============================================================
-- Ejercicio 25 - Funciones con foldr y foldl
-- ============================================================

sumaListaFoldr :: [Int] -> Int
sumaListaFoldr = sum

idL :: [a] -> [a]
idL = foldr (:) []

memberFoldr :: Eq a => a -> [a] -> Bool
memberFoldr x = foldr (\y acc -> y == x || acc) False

appendFoldr :: [a] -> [a] -> [a]
appendFoldr xs ys = foldr (:) ys xs

revFoldr :: [a] -> [a]
revFoldr = foldr (\x acc -> acc ++ [x]) []

norma2 :: [Float] -> Float
norma2 = sqrt . foldr (\x acc -> x*x + acc) 0

flat :: [[a]] -> [a]
flat = concat

insort :: Ord a => [a] -> [a]
insort = foldr insertar []
  where insertar x [] = [x]
        insertar x (y:ys)
          | x <= y    = x : y : ys
          | otherwise = y : insertar x ys

partes :: [a] -> [[a]]
partes = foldr (\x acc -> acc ++ map (x:) acc) [[]]

compFuncs :: [a -> a] -> a -> a
compFuncs = foldr (.) id

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x acc -> if p x then x:acc else acc) []

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr = map

-- Con foldl
sumaListaFoldl :: [Int] -> Int
sumaListaFoldl = sum

revFoldl :: [a] -> [a]
revFoldl = foldl (flip (:)) []

-- sumaAlt: no se puede con foldl porque no sabe cuantos elementos quedan
sumaAlt :: [Int] -> Int
sumaAlt xs = fst $ foldr (\x (acc, sign) -> (acc + sign * x, negate sign)) (0, 1) xs


-- ============================================================
-- Ejercicio 26 - esPrimo y primos mellizos
-- ============================================================

esPrimo :: Int -> Bool
esPrimo n = n > 1 && foldr (\d acc -> n `mod` d /= 0 && acc) True
                           [2..floor (sqrt (fromIntegral n :: Double))]

cantMell :: Int
cantMell = foldr (\(a,b) acc -> if esPrimo a && esPrimo b then acc+1 else acc) 0 pares
  where pares = map (\n -> (n, n+2)) [2..998]


-- ============================================================
-- Ejercicio 27 - foldr'
-- ============================================================

foldr' :: (c -> b -> b) -> (a -> c) -> b -> [a] -> b
foldr' _ _ b [] = b
foldr' f g b (x:xs) = f (g x) (foldr' f g b xs)

sumaListaFoldr' :: [Int] -> Int
sumaListaFoldr' = foldr' (+) id 0

longFoldr' :: [a] -> Int
longFoldr' = foldr' (\_ acc -> acc + 1) (const 1) 0

mapFoldr' :: (a -> b) -> [a] -> [b]
mapFoldr' f = foldr' (:) f []


-- ============================================================
-- Ejercicio 28 - Variantes de foldr
-- ============================================================

-- i) Para listas no vacias
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [x]    = x
foldr1' f (x:xs) = f x (foldr1' f xs)
foldr1' _ []     = error "lista vacia"

maxl :: Ord a => [a] -> a
maxl = foldr1' max

-- ii) Para ArbBin
foldArb :: b -> (b -> a -> b -> b) -> ArbBin a -> b
foldArb base _ Hoja = base
foldArb base f (Nodo izq x der) = f (foldArb base f izq) x (foldArb base f der)

data ArbBinRotHoj a = Leaf a | Branch (ArbBinRotHoj a) (ArbBinRotHoj a) deriving (Show)

foldArbHoj :: (a -> b) -> (b -> b -> b) -> ArbBinRotHoj a -> b
foldArbHoj f _ (Leaf x)     = f x
foldArbHoj f g (Branch l r) = g (foldArbHoj f g l) (foldArbHoj f g r)

-- iii) duplicados
duplicados :: Eq a => [a] -> [a]
duplicados xs = foldr (\x acc ->
  if length (filter (==x) xs) > 1 && x `notElem` acc
  then x:acc else acc) [] xs


-- ============================================================
-- Ejercicio 29 - cant y cantll
-- ============================================================

cantFoldr :: (a -> Bool) -> [a] -> Int
cantFoldr p = foldr (\x acc -> if p x then acc + 1 else acc) 0

cantFoldl :: (a -> Bool) -> [a] -> Int
cantFoldl p = foldl (\acc x -> if p x then acc + 1 else acc) 0

cantll :: (a -> Bool) -> [[a]] -> Int
cantll p = foldr ((+) . cantFoldr p) 0

cantll' :: (a -> Bool) -> [[a]] -> Int
cantll' p = foldr (\xs acc -> cantFoldr p xs + acc) 0


-- ============================================================
-- Ejercicio 30 - appendCond
-- ============================================================

appendCond :: (a -> Bool) -> [[a]] -> [a]
appendCond p = foldr (\xs acc -> if all p xs then xs ++ acc else acc) []

appendCondTol :: (a -> Bool) -> Int -> [[a]] -> [a]
appendCondTol p t = foldr (\xs acc ->
  if length xs >= t && cantFoldr p xs >= t
  then xs ++ acc else acc) []


-- ============================================================
-- Ejercicio 31 - ordenada y llordenada
-- ============================================================

foldr2 :: (a -> a -> b -> b) -> b -> [a] -> b
foldr2 _ b []       = b
foldr2 _ b [_]      = b
foldr2 f b (x:y:xs) = f x y (foldr2 f b (y:xs))

ordenada :: Ord a => (a -> a -> Bool) -> [a] -> Bool
ordenada rel = foldr2 (\x y acc -> rel x y && acc) True

llordenada :: Ord a => (a -> a -> Bool) -> [[a]] -> Bool
llordenada rel xss = ordenada rel (concat xss)


-- ============================================================
-- Ejercicio 32 - foldrN y foldrN2
-- ============================================================


foldrN :: ([a] -> b -> b) -> b -> [[a]] -> b
foldrN _ b [] = b
foldrN f b xss = if null (head xss) then b else f (map head xss) (foldrN f b (map tail xss))

-- FoldrN con guardas

-- foldrN :: ([a] -> b -> b) -> b -> [[a]] -> b
-- foldrN _ b [] = b
-- foldrN f b xss
--   | null (head xss) = b
--   | otherwise = f (map head xss) (foldrN f b (map tail xss))


foldrN2 :: (a -> b -> b) -> b -> [[a]] -> [b]
foldrN2 f b xss = map (foldr f b) (foldrN (:) [] xss)


-- ============================================================
-- Ejercicio 33 - Operaciones matriciales
-- ============================================================

type VectorF = [Float]
type MatrizF = [[Float]]

prodVect :: VectorF -> VectorF -> Float
prodVect v1 v2 = sum (zipWith (*) v1 v2)

prodMatVect :: MatrizF -> VectorF -> VectorF
prodMatVect m v = map (`prodVect` v) m

traspuesta :: MatrizF -> MatrizF
traspuesta = foldrN (:) []

prodMat :: MatrizF -> MatrizF -> MatrizF
prodMat m1 m2 = map (\fila -> map (prodVect fila) nt) m1
  where nt = traspuesta m2
listaPares :: ([a], [b]) -> [[(a, b)]]
listaPares (xs, ys) = map (\i -> map (\j -> (i, j)) ys) xs

prodMat' :: MatrizF -> MatrizF -> MatrizF
prodMat' m1 m2 = map (map (uncurry prodVect)) (listaPares (m1, nt))
  where nt = traspuesta m2
