module SetV2
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where 

data Set a = MKS [a] Int
    deriving Show
{-INV. REP.:
    *Sea MKS xs n
    *n debe equivaler a la cantidad de elementos sin repetir en xs.
-}      

--(addS 1 (addS 2 emptyS))

ejemploSet :: Set String
ejemploSet = addS "Heinze" $ addS "Mascherano" $ addS "Heinze" $ addS "Gago" emptyS

ejemploSet2 :: Set String
ejemploSet2 = addS "Bonano" $ addS "Mascherano" $ addS "Bonano" $ addS "Riquelme" emptyS

emptyS :: Set a
emptyS = MKS [] 0

addS :: Eq a => a -> Set a -> Set a
addS x (MKS ys n) = if (pertenece x ys)
                      then MKS (x:ys) n
                      else MKS (x:ys) (n+1)

belongs :: Eq a => a -> Set a -> Bool
belongs x (MKS ys n) = pertenece x ys 

sizeS :: Eq a => Set a -> Int
sizeS (MKS xs n) = n

removeS :: Eq a => a -> Set a -> Set a
--PRECOND.: El elemento dado debe estar en el conjunto dado.
removeS x (MKS ys n) = MKS (quitar x ys) (n-1)

unionS :: Eq a => Set a -> Set a -> Set a
unionS (MKS xs _) (MKS ys n) = MKS (xs ++ ys) (elemsUnion xs ys n)

setToList :: Eq a => Set a -> [a]
setToList (MKS xs n) = sinRepetidos xs          

--AUXILIARES

pertenece :: Eq a => a -> [a] -> Bool
pertenece _  []    = False
pertenece x (y:ys) = (x==y) || pertenece x ys  

quitar :: Eq a => a -> [a] -> [a]
quitar x []     = []
quitar x (y:ys) = if (x==y)
                    then quitar x ys 
                    else y : (quitar x ys)

elemsUnion :: Eq a => [a] -> [a] -> Int -> Int
elemsUnion []     ys   n = n 
elemsUnion (x:xs) (ys) n = if (pertenece x ys)
                               then elemsUnion xs ys n
                               else elemsUnion xs (x:ys) (n+1)                    

sinRepetidos :: Eq a => [a] -> [a]               
sinRepetidos []     = []
sinRepetidos (x:xs) = if pertenece x xs          
                        then sinRepetidos xs
                        else x : sinRepetidos xs                    