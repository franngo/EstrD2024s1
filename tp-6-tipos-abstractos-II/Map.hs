module Map
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where 

data Map k v = M [(k,v)]
{-INV. REP.: 
    *Sea M xs
    *No pueden existir dos pares en xs con la misma clave/el mismo dato para el primer elemento del par.
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

assocM :: Eq k => k -> v -> Map k v -> Map k v         --COSTO O(n), siendo n la cantidad de elementos presentes en la lista del map dado.
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M xs) = M (asociar k v xs)

lookupM :: Eq k => k -> Map k v -> Maybe v            --COSTO O(n), siendo n la cantidad de elementos presentes en la lista del map dado.
--Propósito: encuentra un valor dada una clave
lookupM k (M xs) = buscar k xs

deleteM :: Eq k => k -> Map k v -> Map k v             --COSTO O(n), siendo n la cantidad de elementos presentes en la lista del map dado.
--Propósito: borra una asociación dada una clave.
--PRECOND= Debe existir una asociación que contenga la clave dada en el map dado.
deleteM k (M xs) = M (borrar k xs)

keys :: Map k v -> [k]                                 --COSTO O(n), siendo n la cantidad de elementos presentes en la lista del map dado.
--Propósito: devuelve las claves del map.
keys (M xs) = claves xs

--AUXILIARES=    

asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]         --COSTO O(n), siendo n la cantidad de elementos presentes en la lista dada.
asociar k v []     = [(k, v)]
asociar k v (x:xs) = if (k==fst(x))
                      then (k,v) : xs
                      else x : asociar k v xs

buscar :: Eq k => k -> [(k,v)] -> Maybe v              --COSTO O(n), siendo n la cantidad de elementos presentes en la lista dada.
buscar k []     = Nothing
buscar k (x:xs) = if (k==fst(x))
                    then (Just (snd(x)))
                    else buscar k xs                

borrar :: Eq k => k -> [(k,v)] -> [(k,v)]               --COSTO O(n), siendo n la cantidad de elementos presentes en la lista dada.  
--PRECOND= Debe existir una asociación que contenga la clave dada en la lista dada.
borrar k []     = error "no hay una asociacion con esa clave en el map dado"
borrar k (x:xs) = if (k==fst(x))
                    then xs
                    else x : borrar k xs

claves :: [(k,v)] -> [k]                                --COSTO O(n), siendo n la cantidad de elementos presentes en la lista dada.
claves []     = []
claves (x:xs) = fst(x) : claves xs   

--data Maybe a = Nothing | Just a
--   deriving Show
