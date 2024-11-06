import Stack

--(push 5(pop(push 4 (push 11 (push 3 emptyS)))))
--interfaz = Stack, emptyS, isEmptyS, push, top, pop, lenS

apilar :: [a] -> Stack a
apilar []     = emptyS
apilar (x:xs) = push x (apilar xs)

desapilar :: Stack a -> [a]
desapilar s = if (isEmptyS s) 
                then []
                else top s : (desapilar (pop s))

insertarEnPos :: Int -> a -> Stack a -> Stack a
--PRECOND= La posición n existe dentro del stack dado.
insertarEnPos 0 x s = push x s
insertarEnPos n x s = push (top s) (insertarEnPos (n-1) x (pop s))