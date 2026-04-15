{- HLINT ignore "Use null" -}
{- HLINT ignore "Use foldr" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use list literal pattern" -}
import Data.Text as Tx(drop, take)
import Prelude hiding (drop)
import Text.Printf (printf)

main = do
  putStrLn "Hello, everybody!"
  putStrLn ("Please look at my favorite odd numbers: " ++ show (filter odd [10..20]))


test = putStrLn "testeooo"

signo :: Int -> Int
signo x
 | x < 0 = -1
 | x > 0 = 1
 | otherwise = 0

negativo :: Int -> Bool
negativo x = signo x == -1

max2 :: Int -> Int -> Int
max2 = max

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 x (max2 y z)

min:: Int -> Int -> Int
min x y = if x > y then y else x

fact :: Int -> Int
fact x = if x == 1 || x == 0 then 1 else x * fact (x-1)

fibo :: Int -> Int
fibo x
 | x == 0 = 0
 | x == 1 = 1
 | otherwise = fibo (x-1) + fibo (x-2)

divpor :: Int -> Int -> Bool
divpor a 0 = False
divpor a b =  mod a b == 0


func = tail [1,2,3,4]

esvacia :: [a] -> Bool
esvacia [a] = length [a] == 0


long  :: [a] -> Int
long [] = 0
long (x:xs) = 1 + long xs

term :: [a] -> Int -> a
term (x:xs) 0 = x
term [] b = error "lista vacia"
term (x:xs) b = if (length xs < b) then error "error indice excedido" else term xs (b-1)

-- forma mas simple sin lsitas auxiliares
revl :: [a] -> [a]
revl [] = []
revl (x:xs) = revl xs ++ [x]

 -- usar prelude Eq
palindromo :: (Eq a) => [a] -> Bool
palindromo xs = xs == revl xs

 -- importante '->' es para funciones explicitas. '=>' es para cualquier tipo a que es de una instancia Eq, ord, NUm etc.

partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) =  partes2 ++ partes3   -- partes2: partes sin x y partes3: partes con x
    where 
      partes2 = partes xs
      partes3  = [x : subparte | subparte <- partes2]


decahex :: Int -> [Char]
decahex a = printf "%X" a