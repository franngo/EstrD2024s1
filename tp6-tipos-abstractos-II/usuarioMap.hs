import Map

--OPERACIONES= Map, emptyM, assocM, lookupM, deleteM, keys
--                  O(1)    O(n)    O(n)     O(n)     O(n)

--assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM
--assocM "Maradona" 1990 $ assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM
--(assocM "Maradona" 1990 $ assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM)
--[("Maradona",1990),("Colon",1492),("Maradona",1986),("Alfonsin",1983)]

ejemploMap :: Map String Int
ejemploMap = assocM "Maradona" 1990 $ assocM "Colon" 1492 $ assocM "Maradona" 1986 $ assocM "Alfonsin" 1983 emptyM

valuesM :: Eq k => Map k v -> [Maybe v]                      --Costo O(n*n), siendo n la cantidad de elementos del map dado.
                                                             --valuesL es de costo O(n*m), pero siendo sus dos parámetros de largo n, es de
                                                             --costo O(n*n)
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = valuesL (keys m) m

valuesL ::  Eq k => [k] -> Map k v -> [Maybe v]              --Costo O(n*m), siendo n la cantidad de elementos de la lista dada y m la
                                                             --cantidad de elementos del map dado.
valuesL []     _ = []
valuesL (k:ks) m = lookupM k m : valuesL ks m

todasAsociadas :: (Eq k, Eq v) => [k] -> Map k v -> Bool     --Costo O(n*m), siendo n la cantidad de elementos de la lista dada y m la
                                                             --cantidad de elementos del map dado. LookupM es lineal en m, y se realiza n veces,
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas []     _ = True
todasAsociadas (k:ks) m = let asociada = lookupM k m /= Nothing
                          in asociada && todasAsociadas ks m 

listToMap :: Eq k => [(k, v)] -> Map k v                     --Costo O(n*(n+2)), siendo n la cantidad de elementos en la lista dada, y el
                                                             --2 las operaciones fst y snd que se suman a la lineal assocM por cada recursión.
--Propósito: convierte una lista de pares clave valor en un map.
listToMap []     = emptyM
listToMap (x:xs) = assocM (fst(x)) (snd(x)) (listToMap xs)

mapToList :: Eq k => Map k v -> [(k, v)]                     --Costo O(n*n+n) (n de keys)
mapToList m = armarListaTuplas (keys m) m  

armarListaTuplas :: Eq k => [k] -> Map k v -> [(k, v)]       --Costo O(n*k, que, como son iguales, es n*n)
armarListaTuplas []     _ = []
armarListaTuplas (k:ks) m = let v = fromJust (lookupM k m)
                            in (k, v) : armarListaTuplas ks m   

agruparEq :: Eq k => [(k, v)] -> Map k [v]              --COSTO O(n*(m+n-1)+m*m), siendo m la cantidad de elementos de la lista de tuplas dada y 
                                                        --n la cantidad de claves sin repetir de la misma. En valoresDeClaves se realizan
                                                        --n*(m+n-1) operaciones constantes y en sinRepes se realizan m*m.
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--la misma clave.    
agruparEq xs = valoresDeClaves (sinRepes xs) xs 

valoresDeClaves :: Eq k => [k] -> [(k, v)] -> Map k [v] --COSTO O(n*m+(n-1)), siendo n la cantidad de elementos de la lista de claves dada
                                                        --y m la cantidad de elementos de la lista de tuplas dada. Es n*m ya que se
                                                        --realizan n operaciones donde, a su vez, se realizan m operaciones constantes
                                                        -- -valoresDeClave- y luego otras n-1 -assocM sobre la parte recursiva-                                                        --
valoresDeClaves []     _  = emptyM
valoresDeClaves (x:xs) ys = let vs = valoresDeClave x ys
                            in assocM x vs (valoresDeClaves xs ys)              

valoresDeClave :: Eq k => k -> [(k,v)] -> [v]           --COSTO O(n), siendo n la cantidad de elementos de la lista de tuplas dada.
valoresDeClave _ []     = []
valoresDeClave x (y:ys) = if (x==(fst(y)))
                            then snd(y) : valoresDeClave x ys
                            else valoresDeClave x ys           

sinRepes :: Eq k => [(k,v)] -> [k]                      --COSTO O(n*n), siendo n la cantidad de elementos presentes en la lista dada.
                                                        --Es n*n porque la cantidad de operaciones en cada recursión es n
sinRepes []     = []
sinRepes (x:xs) = if (estaRepe (fst(x)) xs)
                    then sinRepes xs
                    else fst(x) : sinRepes xs

estaRepe :: Eq k => k -> [(k,v)] -> Bool                --COSTO O(n), siendo n la cantidad de elementos presentes en la lista dada.
estaRepe x []     = False
estaRepe x (y:ys) = x==fst(y) || estaRepe x ys                      

incrementar :: Eq k => [k] -> Map k Int -> Map k Int         --Costo O(n*(n*2)), siendo n la cantidad de elementos presentes en el map dado.
                                                             --Se ejecuta n veces incrementarSi, que tiene costo n*2.
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--cada número asociado con dichas claves. 
incrementar []     m = m
incrementar (x:xs) m = incrementarSi x $ incrementar xs m

incrementarSi :: Eq k => k -> Map k Int -> Map k Int         --Costo O(n*2), siendo n la cantidad de elementos presentes en el map dado.
incrementarSi x m = case (lookupM x m) of
                      Nothing -> m
                      Just n  -> assocM x (n+1) m

--case se usa para "hacer pm del lado derecho" SOLO PARA CUANDO CONOZCO LA REPRESENTACION DE LA ESTRUCTURA
--como con la lista o con el Maybe

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v            --Costo O(n*(m+o)) siendo n la cantidad de claves del primer map dado, m la 
                                                             --cantidad de elementos de este mismo y o la cantidad de elementos del 
                                                             --segundo map dado.
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps m1 m2 = mergeMaps' (keys m1) m1 m2 

mergeMaps' :: Eq k => [k] -> Map k v -> Map k v -> Map k v   --Costo O(n*(m+o)) siendo n la cantidad de elementos de la lista de claves dada,
                                                             --m la cantidad de elementos del primer map dado y o la cantidad de elementos
                                                             --del segundo map dado.
mergeMaps' []     _  m2 = m2
mergeMaps' (k:ks) m1 m2 = let v = fromJust (lookupM k m1)
                          in mergeMaps' ks m1 (assocM k v m2)

indexar :: [a] -> Map Int a                                  --Costo O(n*n-1), siendo n la cantidad de elementos de la lista dada.
--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
--su posición en la lista.
indexar xs = indexar' 1 xs

indexar' :: Int -> [a] -> Map Int a                          --Costo O(n*n-1), siendo n la cantidad de elementos de la lista dada. Se
                                                             --realizan n veces un assocM sobre la parte recursiva -que consta de n-1
                                                             --elementos, y, como assocM es lineal sobre esta, son n-1 ops constantes
                                                             --en cada ejecución-
indexar' _ []     = emptyM                                  
indexar' n (x:xs) = assocM n x (indexar' (n+1) xs)

ocurrencias :: String -> Map Char Int                        --Costo O(n*(m+n-1)+m*m), siendo m la cantidad de elementos del string dado
                                                             --y n la cantidad de caracteres únicos que tiene el mismo. Se realizan 
                                                             --n*(m+n-1) operaciones constantes en ocurrencias' y m*m en sinRepesLista.
--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
--en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias s = ocurrencias' (sinRepesLista s) s 

ocurrencias' :: String -> String -> Map Char Int             --Costo O(n*m+(n-1)), siendo n la cantidad de elementos del primer string dado
                                                             --y m la cantidad de elementos del segundo. Se realizan n veces m operaciones
                                                             -- -ocurrenciasDeEn- además de otras n-1 -assocM-
ocurrencias' []     s = emptyM
ocurrencias' (x:xs) s = let num = ocurrenciasDeEn x s
                        in assocM x num (ocurrencias' xs s)

ocurrenciasDeEn :: Eq a => a -> [a] -> Int                   --Costo O(n), siendo n la cantidad de elementos de la lista dada.
ocurrenciasDeEn _ []     = 0
ocurrenciasDeEn x (y:ys) = if (x==y)
                             then 1 + ocurrenciasDeEn x ys 
                             else ocurrenciasDeEn x ys          

sinRepesLista :: Eq a => [a] -> [a]                          --Costo O(n*n), siendo n la cantidad de elementos de la lista dada. Se 
                                                             --realizan n veces n operaciones -estaRepeLista-
sinRepesLista []     = []
sinRepesLista (x:xs) = if (estaRepeLista x xs)
                         then sinRepesLista xs
                         else x : sinRepesLista xs                       

estaRepeLista :: Eq a => a -> [a] -> Bool                    --Costo O(n), siendo n la cantidad de elementos de la lista dada.
estaRepeLista _ []      = False
estaRepeLista x (y:ys ) = x==y || estaRepeLista x ys                          

--AUXILIARES GENERALES=

fromJust :: Maybe a -> a                                     --Costo O(1)
fromJust Nothing   = error "no deberia dar esto"
fromJust (Just x)  = x 

--data Maybe a = Nothing | Just a
--   deriving Show
--ESTOY USANDO EL MISMO TIPO MAYBE DEL MODULO QUE SE IMPORTA POR DEFECTO SIEMPRE EN HASKELL (Hugs.Prelude.Maybe)                                                     