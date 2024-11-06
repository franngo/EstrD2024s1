import Set

--(addS 1 (addS 2 emptyS))
--interfaz = Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     s = []
losQuePertenecen (x:xs) s = if (belongs x s)
                              then x : (losQuePertenecen xs s)
                              else losQuePertenecen xs s

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (sinRepesConS xs) 

sinRepesConS :: Eq a => [a] -> Set a 
sinRepesConS []     = emptyS
sinRepesConS (x:xs) = addS x (sinRepesConS xs)

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

--(NodeT (addS 1 (addS 2 emptyS)) (NodeT (addS 2 (addS 5 (addS 9 emptyS))) EmptyT EmptyT) (NodeT (addS 3 (addS 8 (addS 9 emptyS))) EmptyT EmptyT) )

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT          = emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))