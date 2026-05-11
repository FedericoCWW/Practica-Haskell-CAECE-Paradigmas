{- HLINT ignore "Use foldr" -}
import Distribution.Simple (Language(Haskell2010))
{- HLINT ignore "Use map" -}
{- HLINT ignore "Eta reduce" -}
doubleList []     = []
doubleList (x:xs) = 2*x : doubleList xs


solonumeros xs = filter(\ x -> x >= 0 && x <= 9) xs

map2 = map.map

-- usar :t para imprimir el tipo de lo que sea.

-- definicon de foldr en Haskell2010

foldr_2 :: (a -> b -> b) -> b -> [a] -> b
foldr_2 f x [] = x
foldr_2 f x (y:ys) = f y (foldr_2 f x ys)


