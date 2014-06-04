import qualified Arr as A
import Arr ((!))
import Seq 

singleton :: a -> A.Arr a 
singleton a = A.tabulate (\_ -> a) 1

take' :: A.Arr a -> Int -> A.Arr a 
take' arr n = A.subArray 0 n arr

drop' :: A.Arr a -> Int -> A.Arr a 
drop' arr n = A.subArray n (A.length arr) arr 


showl' :: A.Arr a -> Seq.ListView a (A.Arr a)
showl' arr | A.length arr == 0 = NIL
    	   | otherwise = CONS (arr ! 0) (drop' arr 1)

showt' :: A.Arr a -> Seq.TreeView a (A.Arr a)
showt' arr | A.length arr == 0 = EMPTY
           | A.length arr == 1 = ELT (arr ! 0) 
           | otherwise = NODE (take' arr n) (drop' arr n)
             where n = div (A.length arr) 2


append :: A.Arr a -> A.Arr a -> A.Arr a
append arr1 arr2 = A.tabulate (append_aux arr1 arr2) ((A.length arr1) + (A.length arr2))

append_aux :: A.Arr a -> A.Arr a -> Int -> a
append_aux arr1 arr2 n = if n >= l1 then arr2 ! (n - l1) else arr1 ! (n) 
                   where l1 = A.length arr1

map_aux :: (a -> b) -> A.Arr a -> Int -> b
map_aux f arr1 n = f (arr1 ! n)

map' :: (a -> b) -> A.Arr a -> A.Arr b
map' f arr = A.tabulate (map_aux f arr) (A.length arr)

filter_aux :: (a -> Bool) -> a -> A.Arr a
filter_aux f a = if f a then singleton a else A.empty

filter' :: (a -> Bool) -> A.Arr a -> A.Arr a
filter' f arr = A.flatten (map' (filter_aux f) arr)

contraer :: A.Arr a -> (a -> a -> a) -> A.Arr a
contraer arr f = if mod l 2 == 0 then A.tabulate g (div l 2) 
                 else append (A.tabulate g (div l 2)) (singleton (arr ! (l-1)))
                   where g = (\i -> f (arr ! (2*i)) (arr ! (2*i+1)))
                         l = A.length arr


reduce :: (a -> a -> a) -> a -> A.Arr a -> a
reduce f b arr = if l1 == 1 then f b (arr ! 0) else reduce f b (contraer arr f)
		           where l1 = A.length arr	


choose :: A.Arr a -> A.Arr a -> (a -> a -> a) -> Int -> a
choose arr1 arr2 f n  | mod n 2 == 0 = arr1 ! (div n 2)
                      | otherwise = f (arr1 ! (div n 2)) (arr2 ! (n-1))

scan :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scan f b arr | l == 1  = (singleton b, f b (arr!0))
             | otherwise = ((A.tabulate (choose (fst (scanr)) arr f)) l, snd (scanr))
                 where l = A.length arr
                       scanr = scan f b (contraer arr f)   


instance Seq A.Arr where 
	emptyS = A.empty
        singletonS a = singleton a 
	lengthS s = A.length s
	nthS s i = (!) s i
	tabulateS f n = A.tabulate f n
	mapS f s = map' f s
	filterS f s = filter' f s
	appendS s s' = append s s'
	takeS s n = take' s n
	dropS s n = drop' s n
	showtS s = showt' s
	showlS s = showl' s
	joinS s = A.flatten s
	reduceS f b s = reduce f b s
	scanS f b s = scan f b s
	fromList l = A.fromList l   
        
