module Set
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where 

data Set a = MKS [a] Int
    deriving Show
{-INV. REP.:
    *Sea MKS xs n
    *Los elementos de xs no pueden ser repetidos.
    *n debe equivaler a la cantidad de elementos en xs.
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
                      then (MKS ys n)
                      else MKS (x:ys) (n+1)


belongs :: Eq a => a -> Set a -> Bool
belongs x (MKS ys n) = pertenece x ys 

sizeS :: Eq a => Set a -> Int                   --COSTO O(1)
sizeS (MKS xs n) = n

removeS :: Eq a => a -> Set a -> Set a
--PRECOND.: El elemento dado debe estar en el conjunto dado.
removeS x (MKS ys n) = MKS (quitar x ys) (n-1)

unionS :: Eq a => Set a -> Set a -> Set a
unionS (MKS xs _) (MKS ys n) = let (elems, num) = unirSinRepes xs ys n
                               in MKS elems num           

setToList :: Eq a => Set a -> [a]
setToList (MKS xs n) = xs          

--AUXILIARES

pertenece :: Eq a => a -> [a] -> Bool
pertenece _  []    = False
pertenece x (y:ys) = (x==y) || pertenece x ys  

quitar :: Eq a => a -> [a] -> [a]
quitar x []     = []
quitar x (y:ys) = if (x==y)
                    then ys
                    else y : (quitar x ys)

unirSinRepes :: Eq a => [a] -> [a] -> Int -> ([a], Int) --COSTO= O(n*m), siendo n la cantidad de elementos en la primera lista y m la 
                                                        --cantidad de elementos en la segunda.
--PRECOND= Las listas no cuentan con elementos repetidos dentro de ellas -aunque pueden tener coincidencias con la otra lista-.
unirSinRepes []     ys   n = (ys, n) 
unirSinRepes (x:xs) (ys) n = if (pertenece x ys)
                               then unirSinRepes xs ys n
                               else unirSinRepes xs (x:ys) (n+1)