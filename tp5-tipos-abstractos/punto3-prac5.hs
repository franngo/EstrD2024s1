import Queue

--(enqueue 7 (enqueue 9 (enqueue 2 emptyQ)))
--interfaz= Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue

lengthQ :: Queue a -> Int
lengthQ q = if (isEmptyQ q)
              then 0
              else 1 + lengthQ (dequeue q)

queueToList :: Queue a -> [a]
queueToList q = if (isEmptyQ q)
                  then []
                  else (firstQ q) : (queueToList (dequeue q))

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if (isEmptyQ q2)
                 then q1 
                 else enqueue (firstQ q2) (unionQ q1 (dequeue q2))   
--observación= Se añaden después de los del 1er queue pero no en orden (al revés de como estaban en su queue original) (quedan adelante los
--mas nuevos y atrás los más viejos). Se podría arreglar con operaciones de interfaz que trabajen sobre el último elemento.                          