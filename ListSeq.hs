--import Seq
import Par
import Data.List

data ListView a t = NIL | CONS a t 
                   deriving Show

singletonS :: a -> ListView a t
singletonS x = CONS x NIL

lengthS :: ListView a t -> Int
lengthS NIL = 0
lengthS (CONS a t) = 1 + lengthS t 

nthS :: ListView a t -> Int -> a
nthS (CONS a t) 0 = a
nthS (CONS a t) n = nthS t (n-1)

tabulateS' :: (Int -> a) -> Int -> Int -> ListView a t
tabulateS' f 0 k = NIL
tabulateS' f n k = CONS (f k) (tabulateS' f (n-1) (k+1))

tabulateS :: (Int -> a) -> Int -> ListView a t
tabulateS f n = tabulateS' f n 0

mapS :: (a -> b) -> ListView a t -> ListView b t
mapS f NIL = NIL
mapS f (CONS a t) = CONS (f a) (mapS f t)

filterS :: (a -> Bool) -> ListView a t -> ListView a t
filterS f NIL = NIL
filterS f (CONS a t) = if f a then CONS a (filterS f t) else filterS f t

appendS :: ListView a t -> ListView a t -> ListView a t
appendS NIL l = l
appendS (CONS a t) l = CONS a (appendS t l) 

takeS :: ListView a t -> Int -> ListView a t
takeS NIL n = NIL
takeS l 0 = NIL
takeS (CONS a t) n = CONS a (takeS t (n-1))

dropS :: ListView a t -> Int -> ListView a t
dropS NIL n = NIL
dropS l 0 = l
dropS (CONS a t) n = dropS t (n-1)

showlS :: ListView a t -> ListView a (ListView a t)
showlS NIL = CONS NIL NIL
showlS (CONS a t) = CONS (CONS a NIL) (showlS t)

joinS :: ListView a (ListView a t) -> ListView a t
joinS (CONS NIL NIL) = NIL
joinS (CONS a t) = appendS a (joinS t)

reduceS :: (a -> a -> a) -> a -> ListView a t -> a
reduceS f e NIL = e
reduceS f e (CONS a t) = f a (reduceS f e t)

{-let (a,v ) = (reduceS f e (take lengthS(cons a t)/2, drop
in f a v-}

--scanS :: (a -> a -> a) -> a -> ListView a t -> (ListView a t, a)
--scanS f 

fromList :: [a] -> ListView a t 
fromList [] = NIL
fromList [x] = CONS x NIL
fromList (x:xs) = let (ls, rs) = splitAt (div (length (x:xs)) 2) (x:xs)
                      (ls', rs') = (fromList ls ||| fromList rs)
                  in appendS ls' rs'





