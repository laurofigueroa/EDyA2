import Seq

singleton :: a -> [a]
singelton a = [a]

nth :: [a] -> Int -> a
nth (x:xs) 0 = x
nth (x:xs) n = nth xs (n-1)

tabulate' :: (Int -> a) -> Int -> Int -> [a]
tabulate' f n 0 = [f n]
tabulate' f n m = [f n] ++ tabulate' f (n+1) (m-1)

tabulate :: (Int -> a) -> Int -> [a]
tabulate f n = tabulate' f 0 n

map' :: (b -> a) -> [b] -> [a]
map' f [] = []
map' f (x:xs) = (f x) : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

append' :: [a] -> [a] -> [a]
append' [] xs = xs
append' (x:xs) ys = x : append' xs ys 

take' :: [a] -> Int -> [a] 
take' xs 0 = []
take' (x:xs) n = x : take' xs (n-1)

drop' :: [a] -> Int -> [a]
drop' xs 0 = xs
drop' (x:xs) n = drop' xs (n-1)

showt :: [a] -> TreeView a [a]
showt [] = EMPTY
showt [x] = ELT x
showt xs = let (lt, rt) = splitAt (div (length' xs) 2) xs
           in NODE lt rt 

showl :: [a] -> ListView a [a]
showl [] = NIL
showl (x:xs) = CONS x xs

join' :: [[a]] -> [a]
join' [] = []
join' (x:xs) = append' x (join' xs)

reduce' :: (a -> a -> a) -> a -> [a] -> a
reduce' f b [] = b
reduce' f b (x:xs) = f x (reduce f b xs)

scan' ::  (a -> a -> a) -> a -> [a] -> ([a], a)
scan' f b [] = 




