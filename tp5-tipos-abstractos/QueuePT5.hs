module QueuePT5
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where 

data Queue a = MKQ [a] [a]
    deriving Show
{-INV. REP.:
    *Sea MKQ fs bs
    *Si fs se encuentra vacía, toda la queue se encuentra vacía.
-OBSERVACIONES=    
    *En esta implementación, los elementos de fs son todos anteriores/más antiguos que los de bs.
    *fs contiene elementos ordenados del más antiguo al más reciente, mientras que los de bs van de más recientes a más antiguos.
-}

--(enqueue 7 (enqueue 9 (enqueue 2 emptyQ)))
--(enqueue 7 (enqueue 9 (enqueue 2(enqueue 8 emptyQ))))

emptyQ :: Queue a                                                --Costo O(1)
emptyQ = MKQ [] []

isEmptyQ :: Queue a -> Bool                                      --Costo O(1)
isEmptyQ (MKQ fs _) = null fs

enqueue :: a -> Queue a -> Queue a                               --Costo O(1)
enqueue x (MKQ [] bs) = MKQ [x] []
enqueue x (MKQ fs bs) = MKQ fs (x:bs)

firstQ :: Queue a -> a                                           --Costo O(1)
--PRECOND= fs no se encuentra vacía.
firstQ (MKQ fs _) = head fs  

dequeue :: Queue a -> Queue a
dequeue (MKQ [] [])     = error "no puede desencolar aca"        --Costo O(n), pero, en promedio, O(1)
dequeue (MKQ (f:[]) []) = MKQ [] []
dequeue (MKQ (f:[]) bs) = MKQ (reverse bs) [] --única operación lineal, la cual es amortizada al no ser el caso más recurrente.
dequeue (MKQ fs bs)     = MKQ (tail fs) bs