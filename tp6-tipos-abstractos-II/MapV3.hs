module MapV3
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = M [k] [v]
{-INV. REP.:
    *Sea M xs ys
    *xs e ys deben tener la misma longitud
    *no debe haber claves repetidas en xs
-}
    deriving Show

--assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM
--assocM "Maradona" 1990 $ assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM
--(assocM "Maradona" 1990 $ assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM)

ejemploMap :: Map String Int
ejemploMap = assocM "Maradona" 1990 $ assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM

emptyM :: Map k v                                      --COSTO O(1)
--Propósito: devuelve un map vacío
emptyM = M [] []

assocM :: Eq k => k -> v -> Map k v -> Map k v      --Costo O(n), siendo n la cantidad de elementos tanto en xs 
                                                    --como en ys. Es lineal porque la operación realizada, assocL,
                                                    -- es lineal.
assocM x y (M xs ys) = let (ks, vs) = assocL x y xs ys   
                       in M ks vs

lookupM :: Eq k => k -> Map k v -> Maybe v         --Costo O(n), siendo n la cantidad de elementos tanto en xs como en ys.
lookupM x (M xs ys) = lookupL x xs ys

deleteM :: Eq k => k -> Map k v -> Map k v          --Costo O(n), siendo n la cantidad de elementos tanto en xs como ys.
--PRECOND= Debe existir una asociación que contenga la clave dada en el map dado.
deleteM x (M xs ys) = let (ks, vs) = deleteL x xs ys
                      in M ks vs

keys :: Map k v -> [k]                              --Costo O(1)
keys (M xs ys) = xs                     

--AUXILIARES:

assocL :: Eq k => k -> v -> [k] -> [v] -> ([k],[v]) --Costo O(n), ya que en cada recursión se realiza un if y dos cons directamente, o 
                                                    --dos cons en agregar. Esto se realiza n veces, siendo n la cantidad de elementos 
                                                    --tanto en ks como en vs
--PRECOND= Las 2 listas dadas deben ser de la misma longitud.                                                    
assocL x y []     _      = ([x],[y])
assocL x y (k:ks) (v:vs) = if (x==k)
                             then ((x:ks), (y:vs))
                             else agregar k v (assocL x y ks vs)

agregar :: k -> v -> ([k],[v]) -> ([k],[v])        --Costo O(1)
agregar x y (ks,vs)= ((x:ks),(y:vs))      

--f2 recorre xs e ys al mismo tiempo. Cuando encuentra
--a x en xs, agrega a y en ys, de modo que la posicion del agregado coincida con la posicion en la que
--se agregó x en la otra función.

--con let se crea variable local con la que podés renombrar función larga. se continúa luego con in.
--entiendo que cuando creas una variable con let, calcula lo que le estás asignando para luego guardarlo en la variable.

lookupL :: Eq k => k -> [k] -> [v] -> Maybe v     --Costo O(n), siendo n la cantidad de elementos tanto en ks como en vs.
--PRECOND= Las 2 listas dadas deben ser de la misma longitud.
lookupL x []     _      = Nothing
lookupL x (k:ks) (v:vs) = if (x==k)
                            then Just v
                            else lookupL x ks vs

deleteL :: Eq k => k -> [k] -> [v] -> ([k],[v])     --Costo O(n), siendo n la cantidad de elementos tanto en ks como vs.
--PRECOND= Debe existir una asociación que contenga la clave dada en la lista dada.
deleteL x []     _      = error "No existe una asociacion con esa clave"
deleteL x (k:ks) (v:vs) = if (x==k)
                           then (ks, vs)
                           else agregar k v (deleteL x ks vs)

--data Maybe a = Nothing | Just a
--   deriving Show                           