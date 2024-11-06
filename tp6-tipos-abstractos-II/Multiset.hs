module MultiSet
    (MultiSet, emptyMS, addMS, ocurrencesMS)
where

import Map

--OPERACIONES DE MAP= Map, emptyM, assocM, lookupM, deleteM, keys
--                         O(1)    O(n)    O(n)     O(n)     O(n)

ejemploMS :: MultiSet String
ejemploMS = addMS "Sekou Mara" $ addMS "Carroll" $ addMS "Alcaraz" $ addMS "Carroll" $ addMS "Sekou Mara" emptyMS

ejemploMS2 :: MultiSet String
ejemploMS2 = addMS "Sekou Mara" $ addMS "Sekou Mara" $ addMS "Alcaraz" $ addMS "Sekou Mara" $ addMS "Smallbone" emptyMS

data MultiSet a = MS (Map a Int)
    deriving Show

emptyMS :: MultiSet a                                    --Costo O(1)
--Propósito: denota un multiconjunto vacío.
emptyMS = MS (emptyM)

addMS :: Ord a => a -> MultiSet a -> MultiSet a          --Costo O(n*2), siendo n la cantidad de elementos presentes en el map del
                                                         --multiset dado.
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS x (MS (m)) = MS (addMS' x m)

addMS' :: Ord a => a -> Map a Int -> Map a Int           --Costo O(n*2), siendo n la cantidad de elementos presentes en el map dado.
                                                         --Se realizan 2 operaciones, las cuales son lineales sobre n en el peor de
                                                         --los casos -lookupM y assocM-
addMS' x m =  if (lookupM x m==Nothing)
               then assocM x 1 m
               else let ocur = fromJust (lookupM x m)
                    in assocM x (ocur+1) m                        

ocurrencesMS :: Ord a => a -> MultiSet a -> Int          --Costo O(n), siendo n la cantidad de elementos en el map del multiset dado. La 
                                                         --operación lineal es lookupM
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
--elemento en el multiconjunto.
ocurrencesMS x (MS (m)) = case (lookupM x m) of
                            Nothing -> 0
                            Just n -> n

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a           --Costo O((n*n)+(n*m*2)+n), siendo n la cantidad de elementos del
                                                                     --map m1 y m la cantidad de elementos del map m2.
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
--ambos multiconjuntos.                            
unionMS (MS (m1)) (MS (m2)) = MS (unionMS' (keys m1) m1 m2)

unionMS' :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int      --Costo O((n*n)+(n*m*2)), siendo n la cantidad de elementos de la lista
                                                                     --dada y del map m1, y siendo m la cantidad de elementos del map m2.
                                                                     --n*n es el costo de lookupM sobre m1 y n*m*2 es el costo de lookupM
                                                                     --y assocM sobre m2
unionMS' []      _ m2 = m2
unionMS' (k:ks) m1 m2 = let vm1 = fromJust (lookupM k m1)
                        in case (lookupM k m2) of
                             Nothing  -> unionMS' ks m1 (assocM k vm1 m2)
                             Just vm2 -> unionMS' ks m1 (assocM k (vm1+vm2) m2) 

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a    --Costo O((n*n)+(n*m*2)+n), siendo n la cantidad de elementos del
                                                                     --map m1 y m la cantidad de elementos del map m2.
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
--multiconjuntos tienen en común.
intersectionMS (MS (m1)) (MS (m2)) = MS (intersectionMS' (keys m1) m1 m2)    

intersectionMS' :: Ord a => [a] -> Map a Int -> Map a Int ->  Map a Int  --Costo O((n*n)+(n*m*2)), siendo n la cantidad de elementos de la 
                                                                         --lista dada y del map m1, y siendo m la cantidad de elementos del
                                                                         --map m2. n*n es el costo de lookupM sobre m1 y n*m*2 es el costo
                                                                         --de lookupM y assocM sobre m2 -peor caso-
intersectionMS' []     _  _  = emptyM
intersectionMS' (k:ks) m1 m2 = let vm1 = fromJust (lookupM k m1)
                               in case (lookupM k m2) of
                                    Nothing  -> intersectionMS' ks m1 m2
                                    Just vm2 -> assocM k (menor vm1 vm2) (intersectionMS' ks m1 m2)
 
multiSetToList :: Eq a => MultiSet a -> [(a, Int)]                   --Costo O(n*n+n), siendo n la cantidad de elementos del map dado.
                                                                     --Se realizan n operaciones lineales sobre el map dado de n elementos
                                                                     -- en multiSetToList', además de otra operación lineal sobre el map
                                                                     -- en keys
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
--su cantidad de ocurrencias.
multiSetToList (MS (m)) = multiSetToList' (keys m) m 

multiSetToList' :: Eq a => [a] -> Map a Int -> [(a, Int)]            --Costo O(n*n), siendo n la cantidad de elementos de la lista de
                                                                     --claves dada y del map dado. Se realizan n operaciones lineales
                                                                     --sobre el map dado -que tiene n elementos-
multiSetToList' []     _ = []
multiSetToList' (k:ks) m = let vm = fromJust (lookupM k m)
                           in (k, vm) : multiSetToList' ks m

--AUXILIARES GENERALES=

menor :: Ord a => a -> a -> a
menor x y = if (x<=y)
              then x
              else y

fromJust :: Maybe a -> a                                 --Costo O(1)
fromJust Nothing   = error "no deberia dar esto"
fromJust (Just x)  = x 

--data Maybe a = Nothing | Just a
--   deriving Show
--ESTOY USANDO EL MISMO TIPO MAYBE DEL MODULO QUE SE IMPORTA POR DEFECTO SIEMPRE EN HASKELL (Hugs.Prelude.Maybe)                       