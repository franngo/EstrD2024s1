module PriorityQueue 
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where 

data PriorityQueue a = PQ [a]
{-INV. REP.: 
    *Sea PQ xs.
    *Los elementos de xs se encuentran ordenados de menor a mayor.
-}    
    deriving Show

--insertPQ 9 $ insertPQ 21 $ insertPQ 2 $ insertPQ 64 $ insertPQ 8 $ insertPQ (-2) $ insertPQ 9 emptyPQ    

emptyPQ :: PriorityQueue a                                   --COSTO O(1)
--Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool                         --COSTO O(1)
--Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --COSTO O(n), siendo n la cantidad de elementos presentes en la lista dada.
--Propósito: inserta un elemento en la priority queue.
insertPQ x (PQ ys) = PQ (agregar x ys)

findMinPQ :: PriorityQueue a -> a                   --COSTO O(1)
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--PRECOND= La PQ dada debe contar con al menos un elemento.
findMinPQ (PQ (x:xs)) = x

deleteMinPQ :: PriorityQueue a -> PriorityQueue a   --COSTO O(1)
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--PRECOND= La PQ dada debe contar con al menos un elemento.
deleteMinPQ (PQ (x:xs)) = PQ xs

--AUXILIARES

agregar :: Ord a => a -> [a] -> [a]                           --COSTO O(n), siendo n la cantidad de elementos presentes en la lista dada.
agregar x []     = [x]
agregar x (y:ys) = if (x <= y)
                     then x:y:ys
                     else y : agregar x ys