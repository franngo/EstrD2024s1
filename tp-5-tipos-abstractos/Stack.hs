module Stack
    (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where 

data Stack a = MKS [a]
    deriving Show
{-INV. REP.:
    *Sea MKS xs
    *Los elementos de xs van de ingresados hace menor tiempo a ingresados hace mayor tiempo
-}

--(push 4 (push 11 (push 3 emptyS)))
--(push 5(pop(push 4 (push 11 (push 3 emptyS)))))

emptyS :: Stack a
emptyS = MKS []

isEmptyS :: Stack a -> Bool
isEmptyS (MKS xs) = null xs

push :: a -> Stack a -> Stack a
push x (MKS ys) = MKS (x:ys)

top :: Stack a -> a
top (MKS xs) = head xs

pop :: Stack a -> Stack a
pop (MKS xs) = MKS (tail xs)

lenS :: Stack a -> Int
lenS (MKS xs) = length xs