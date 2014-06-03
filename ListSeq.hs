import Seq
import Par

singleton :: a -> [a]
singleton a = [a]

nth :: [a] -> Int -> a
nth (x:xs) 0 = x
nth (x:xs) n = nth xs (n-1)

tabulate :: (Int -> a) -> Int -> [a]
tabulate f 0 = []
tabulate f 1 = [f 0]
tabulate f n = let (a, b) = (tabulate f (div n 2) ||| tabulate (f.(+(div n 2))) (n - div n 2))
                    in a ++ b

map' :: (b -> a) -> [b] -> [a]
map' f [] = []
map' f (x:xs) = (f x) : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x then x : (filter' f xs) else filter' f xs

append' :: [a] -> [a] -> [a]
append' [] xs = xs
append' (x:xs) ys = x : append' xs ys 

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

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

contraer :: [a] -> (a -> a -> a) -> [a]
contraer xs f = if (mod l 2 == 0) then  tabulate g (div l 2)
                else append' (tabulate g (div l 2)) (singleton (nth xs (l-1)))
                where g = (\i -> f (nth xs (2*i)) (nth xs (2*i+1)))
                      l = length' xs

reduce' :: (a -> a -> a) -> a -> [a] -> a
reduce' f b [] = b
reduce' f b (x:xs) = f x (reduce' f b xs)

reduce :: (a -> a -> a) -> a -> [a] -> a
reduce f b [x] = f b x
reduce f b (x:xs) = reduce f b (contraer (x:xs) f)

--scan' ::  (a -> a -> a) -> a -> [a] -> ([a], a)
--scan' f b [] = 




