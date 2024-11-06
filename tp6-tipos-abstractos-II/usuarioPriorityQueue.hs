import PriorityQueue

--OPERACIONES= emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ

heapSort :: Ord a => [a] -> [a]                      --COSTO O(n^2+n), siendo n la cantidad de elementos presentes en la lista dada.
                                                     --Se realiza una operación cuadrática sobre n que es ordenarConPQ y otra lineal que es enlistarPQ.
heapSort xs = enlistarPQ (ordenarConPQ xs)

ordenarConPQ :: Ord a => [a] -> PriorityQueue a      --COSTO O(n^2), siendo n la cantidad de elementos presentes en la lista dada.
                                                     --Se realiza una operación lineal por cada elemento presente en la estructura, ya que insertPQ es una operación lineal.
ordenarConPQ []     = emptyPQ 
ordenarConPQ (x:xs) = insertPQ x (ordenarConPQ xs)

enlistarPQ :: PriorityQueue a -> [a]                 --COSTO O(n), siendo n la cantidad de elementos presentes en la PQ dada.
enlistarPQ pq = if (isEmptyPQ pq)
                  then []
                  else findMinPQ pq : enlistarPQ (deleteMinPQ pq)