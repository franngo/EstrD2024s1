module MapV2
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where 

data Map k v = M [(k,v)]
{-INV. REP.: 
    *No tiene.
-}    
    deriving Show

--assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM
--assocM "Maradona" 1990 $ assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM
--(assocM "Maradona" 1990 $ assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM)

ejemploMap :: Map String Int
ejemploMap = assocM "Maradona" 1990 $ assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM

emptyM :: Map k v                                      --COSTO O(1)
--Propósito: devuelve un map vacío
emptyM = M []

assocM :: k -> v -> Map k v -> Map k v                 --COSTO O(1)
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M xs) = M ((k,v):xs)

lookupM :: Eq k => k -> Map k v -> Maybe v            --COSTO O(n), siendo n la cantidad de elementos presentes en la lista del map dado.
--Propósito: encuentra un valor dada una clave
lookupM k (M xs) = buscar k xs

deleteM :: Eq k => k -> Map k v -> Map k v             --COSTO O(n), siendo n la cantidad de elementos presentes en la lista del map dado.
--Propósito: borra al menos una asociación dada una clave.
--PRECOND= Debe existir al menos una asociación que contenga la clave dada en el map dado.
deleteM k (M xs) = M (borrar k xs)

keys :: Eq k => Map k v -> [k]                         --COSTO O(n*n), siendo n la cantidad de elementos presentes en la lista dada.
--Propósito: devuelve las claves del map.
keys (M xs) = sinRepes xs

--AUXILIARES=

buscar :: Eq k => k -> [(k,v)] -> Maybe v              --COSTO O(n), siendo n la cantidad de elementos presentes en la lista dada.
buscar k []     = Nothing
buscar k (x:xs) = if (k==fst(x))
                    then (Just (snd(x)))
                    else buscar k xs  

borrar :: Eq k => k -> [(k,v)] -> [(k,v)]               --COSTO O(n), siendo n la cantidad de elementos presentes en la lista dada.  
--PRECOND= Debe existir al menos una asociación que contenga la clave dada en la lista dada.
borrar k []     = []
borrar k (x:xs) = if (k==fst(x))
                    then borrar k xs
                    else x : borrar k xs 

sinRepes :: Eq k => [(k,v)] -> [k]                      --COSTO O(n*n), siendo n la cantidad de elementos presentes en la lista dada.
                                                        --Es n*n porque la cantidad de operaciones en cada recursión es n
sinRepes []     = []
sinRepes (x:xs) = if (estaRepe (fst(x)) xs)
                    then sinRepes xs
                    else fst(x) : sinRepes xs

estaRepe :: Eq k => k -> [(k,v)] -> Bool                --COSTO O(n), siendo n la cantidad de elementos presentes en la lista dada.
estaRepe x []     = False
estaRepe x (y:ys) = x==fst(y) || estaRepe x ys  

--data Maybe a = Nothing | Just a
--   deriving Show