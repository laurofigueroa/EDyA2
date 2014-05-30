
data Bin a = Hoja | Node (Bin a) a (Bin a) deriving Show

insert :: Ord a => a -> Bin a -> Bin a
insert a Hoja = Node Hoja a Hoja
insert a (Node l b r) | a <= b = Node (insert a l) b r
                      | otherwise = Node l b (insert a r)
