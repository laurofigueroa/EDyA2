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
lookup2 k (Node l s (key,a) r)  | k == key = Just a
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


