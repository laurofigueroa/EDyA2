import qualified Arr as A
import Arr (!)

singleton :: a -> Arr a
singleton a = fromList [a]

take' :: Arr a -> Int -> Arr a
take' arr n = subarray 0 n arr

drop' :: Arr a -> Int -> Arr a
drop arr n = subarray n (length arr) arr

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
             in append (singleton a) b 




