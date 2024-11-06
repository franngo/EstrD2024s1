module QueueV2
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where 

data Queue a = MKQ [a]
    deriving Show
{-INV. REP.:
    *Sea MKQ xs
-OBSERVACIONES=      
    *En esta implementación, los elementos de xs van de ingresados hace menor tiempo a ingresados hace mayor tiempo.
-}

--(enqueue 7 (enqueue 9 (enqueue 2 emptyQ)))

emptyQ :: Queue a
emptyQ = MKQ []

isEmptyQ :: Queue a -> Bool
isEmptyQ (MKQ xs) = null xs

enqueue :: a -> Queue a -> Queue a
enqueue x (MKQ xs) = MKQ ((x:xs))

firstQ :: Queue a -> a
--PRECOND= xs no se encuentra vacía.
firstQ (MKQ xs) = last xs

dequeue :: Queue a -> Queue a
dequeue (MKQ []) = error "no puede desencolar aca"
dequeue (MKQ xs) = MKQ (sacarElUltimo xs)

--AUXILIARES=

sacarElUltimo :: [a] -> [a]
sacarElUltimo (x:[]) = []
sacarElUltimo (x:xs) = x : (sacarElUltimo xs)