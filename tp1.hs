import Data.Maybe


class Diccionario t where
  vacia    :: Ord k => t k v
  insertar :: Ord k => (k, v) -> t k v -> t k v
  eliminar :: Ord k => k -> t k v -> t k v
  buscar   :: Ord k => k -> t k v -> Maybe v 
  
data BTree32 k a = Nil --arbol vacio
                 | Node
                    (BTree32 k a) --subarbol izquierdo
                    Int           --tamaño del arbol
                    (k,a)         --elemento del nodo
                    (BTree32 k a) deriving Show --subarbol derecho
                    
                    
size :: BTree32 k a -> Int
size Nil = 0
size (Node l s (k, a) r) = s
                    
lookup2 :: Ord k => k -> BTree32 k a -> Maybe a
lookup2 k Nil = Nothing
lookup2 k (Node Nil s (key,a) Nil) = if k == key then Just a else Nothing
lookup2 k (Node Nil s (key,a) r) = if k == key then Just a else lookup2 k r
lookup2 k (Node l s (key,a) Nil) = if k == key then Just a else lookup2 k l
lookup2 k (Node l s (key,a) r) 
    | k == key = Just a
    | k < key = lookup2 k l
    | k > key = lookup2 k r

singleL :: BTree32 k a -> BTree32 k a
singleL Nil = Nil
singleL (Node l s (key, a) Nil) = Node l s (key, a) Nil
singleL (Node aleft s (key, a) (Node c s' (key', a') d)) = (Node (Node aleft s (key, a) c) s' (key', a') d)

doubleL :: BTree32 k a -> BTree32 k a
doubleL Nil = Nil
doubleL (Node l s (key, a) (Node (Node cl s1 (key1, a1) cr) s' (key', a') d)) = (Node (Node l s (key, a) cl) s1 (key1, a1) (Node cr s' (key',a') d))

singleR :: BTree32 k a -> BTree32 k a
singleR Nil = Nil 
singleR (Node Nil s (key, a) r) = Node Nil s (key, a) r
singleR (Node (Node aleft s (key, a) c) s' (key', a') d) = (Node aleft s (key, a) (Node c s' (key', a') d))

doubleR :: BTree32 k a -> BTree32 k a
doubleR (Node (Node al as (akey, a) (Node cl cs (ckey, c) cr)) bs (bkey, b) br) = (Node (Node al as (akey, a) cl) cs (ckey, c) (Node cr bs (bkey, b) br))

isBalanced :: BTree32 k a -> Bool
isBalanced Nil = True
isBalanced (Node l s (k, a) r) = if size r <= 3 * size l && size l <= 3 * size r && ((size r == 0 && size l == 1) || (size l == 0 && size r == 1)) then True else False

balance :: BTree32 k a -> (k, a) ->  BTree32 k a -> BTree32 k a
balance (t1 @ (Node l1 s1 (k1, a1) r1)) (k, a) (t2 @ (Node l2 s2 (k2, a2) r2)) | size t1 + size t2 <= 1 || (isBalanced t1 && isBalanced t2) = Node t1 (s1+s2+1) (k, a) t2
                                                                               | not(size t2 <= 3 * size t1) = if size l2 < 2 * size r2 then singleL (Node t1 (s1+s2+1) (k, a) t2) else doubleL (Node t1 (s1+s2+1) (k, a) t2)
                                                                               | not(size t1 <= 3 * size t2) = if size l1 < 2 * size r1 then singleR (Node t1 (s1+s2+1) (k, a) t2) else doubleR (Node t1 (s1+s2+1) (k, a) t2)        



-- LAURO

insert :: Ord k => (k, a) -> BTree32 k a -> BTree32 k a
intser (k, a) Nil = (Node Nil 1 (k, a) Nil)
insert (newk, newv) (Node l s (k, a) r) | newk <= k = balance (Node l (s+1) (newk, newv) Nil) (k, a) r
                                        | newk > k  = balance l (k, a) (Node Nil (s+1) (newk, newv) r)
                                        | otherwise = (Node l s (k, a) r)

