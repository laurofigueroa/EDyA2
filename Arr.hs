{-
 Este módulo requiere la librería Vector. 
 
 Esta puede instalarse utilizando Cabal, ejecutando el siguiente código  
 en un intérprete de comandos: 
 
 $ update cabal
 $ cabal install vector
 
-}

module Arr (Arr, length, tabulate, (!), subArray, fromList, flatten) where

import Prelude hiding (length)
import qualified Data.Vector as V
import Control.Monad
import Par

newtype Arr a = A {getVector ::  V.Vector a }


length    :: Arr a -> Int
length (A p)  = V.length p

(!) :: Arr a -> Int -> a
(!) (A p) i = (V.!) p i

subArray :: Int ->  -- índice inicial
            Int ->  -- tamaño
            Arr a -> Arr a
subArray i n (A p) = A (V.slice i n p)             
             

tabulate :: (Int -> a) -> Int -> Arr a
tabulate f n  = A (V.generate n f)

fromList :: [a] -> Arr a
fromList xs = A (V.fromList xs)

empty :: Arr a
empty = A (V.empty)

flatten :: Arr (Arr a) -> Arr a
flatten (A pa) = A (join (fmap getVector pa))

instance Show a => Show (Arr a) where
         show p = "<"++ show' p (length p) 0
            where show' p n i | i== n     = ">"
                              | i== (n-1) = show (p ! i) ++ ">"
                              | otherwise = show (p ! i) ++ ","++ show' p n (i+1)
                              

singleton :: a -> Arr a 
singleton a = A (V.singleton a)

take' :: Arr a -> Int -> Arr a 
take' arr n = subArray 0 n arr

drop' :: Arr a -> Int -> Arr a 
drop' arr n = subArray n (length arr) arr 

{-
showl' :: Arr a -> ListView a (Arr a)
showl' empty = NIL
showl' arr = CONS (arr ! 0) (drop arr 1)

showt' :: Arr a -> TreeView a (Arr a)
showt' empty = EMPTY
showt' arr = if length arr == 1 then ELT (arr ! 0) else NODE (take n arr) (drop n arr)
             where n = div (length arr) 2

map' :: (a -> b) -> Arr a -> Arr b
map' _ empty = empty
map' f arr = let (a,b) = (f (arr ! 0) ||| map' f (drop arr 1)) 
             in append a b  

-}


append :: Arr a -> Arr a -> Arr a
append arr1 arr2 = tabulate (append_aux arr1 arr2) ((length arr1) + (length arr2))

append_aux arr1 arr2 n = if n >= l1 then arr2 ! (n - l1) else arr1 ! (n) 
                       where l1 = length arr1

map' f arr = tabulate (map_aux f arr) (length arr)

map_aux f arr1 n = f (arr1 ! n)


filter_aux f a = if f a then singleton a else empty

filter' f arr = flatten (map' (filter_aux f) arr)

contraer :: Arr a -> (a -> a -> a) -> Arr a
contraer arr f = if mod l 2 == 0 then tabulate g (div l 2) 
                 else append (tabulate g (div l 2)) (singleton (arr ! (l-1)))
                  where g = (\i -> f (arr ! (2*i)) (arr ! (2*i+1)))
                        l = length arr


reduce :: (a -> a -> a) -> a -> Arr a -> a
reduce f b arr = if l1 == 1 then f b (arr ! 0) else reduce f b (contraer arr f)
		            where l1 = length arr	

expandir :: Arr a -> (a -> a -> a) -> Arr a
expandir arr f | length arr2 > 1 = append (singleton (arr2 ! 0)) (expandir arr2 f)
               | length arr2 == 1 = singleton (arr2 ! 0) 
               | otherwise = empty
                   where arr2 = contraer arr f

choose :: Arr a -> Arr a -> (a -> a -> a) -> a -> Int -> a
choose arr1 arr2 f b n | n == 0 = b
                       | n == 1 = f b (arr2 ! (n-1))
                       | mod n 2 == 0 = f b (arr1 ! (div n 2))
                       | otherwise = f b (f (arr1 ! (div n 2)) (arr2 ! (n-1)))


scan :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
scan f b arr = let  l = length arr
                    t = tabulate (choose (append (singleton b) (expandir arr f)) arr f b) (l+1)
                    in (take' t l, (t ! l)) 
                    
