{-

--EJERCICIO 1

--OPERACIONES= emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ
--                                 O(log n)  O(1)       O(log n)

heapSort :: Ord a => [a] -> [a]                      --COSTO O(n*log n*2), siendo n la cantidad de elementos presentes en la lista dada.
                                                     --Se realiza una operación cuadrática sobre n que es ordenarConPQ y otra lineal que es enlistarPQ.
heapSort xs = enlistarPQ (ordenarConPQ xs)

ordenarConPQ :: Ord a => [a] -> PriorityQueue a      --COSTO O(n*log n), siendo n la cantidad de elementos presentes en la lista dada.
                                                     --Se realiza una operación logarítmica por cada elemento presente en la estructura,
                                                     --ya que insertPQ es una operación logarítmica.
ordenarConPQ []     = emptyPQ 
ordenarConPQ (x:xs) = insertPQ x (ordenarConPQ xs)

enlistarPQ :: PriorityQueue a -> [a]                 --COSTO O(n*log n), siendo n la cantidad de elementos presentes en la PQ dada.
                                                     --Se realiza una operación logarítmica por cada elemento presente en la estructura,
                                                     --ya que deleteMinPQ es una operación logarítmica.
enlistarPQ pq = if (isEmptyPQ pq)
                  then []
                  else findMinPQ pq : enlistarPQ (deleteMinPQ pq)

-}                  

--EJERCICIO 2                  

ejBSTyAVL :: Tree Int 
ejBSTyAVL = (NodeT 11 (NodeT 9 (NodeT 7 EmptyT EmptyT) (NodeT 10 EmptyT EmptyT)) (NodeT 15 (NodeT 13 EmptyT EmptyT) (NodeT 16 EmptyT EmptyT)) )

ejNOBSTyAVL :: Tree Int
ejNOBSTyAVL = (NodeT 11 (NodeT 9 (NodeT 7 EmptyT EmptyT) (NodeT 12 EmptyT EmptyT)) (NodeT 15 (NodeT 13 EmptyT EmptyT) (NodeT 16 EmptyT EmptyT)) )

ejBSTyNOAVL :: Tree Int
ejBSTyNOAVL = (NodeT 11 EmptyT (NodeT 15 (NodeT 13 EmptyT EmptyT) (NodeT 16 EmptyT EmptyT)) )

belongBST ::  Ord a => a -> Tree a -> Bool           --Costo : O(log N), siendo N la cantidad total de elementos en el tree dado,
                                                     --ya que se recorre solamente una rama entera del árbol -peor caso- haciendo 
                                                     --operaciones constantes, o sea, log n operaciones constantes. 
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--PRECOND: cumple los invariantes de BST y no tiene elementos repetidos.
--Observación= Para la medición de costos, asumimos que el árbol siempre se encuentra balanceado tras cada operación.
belongBST x EmptyT          = False
belongBST x (NodeT y ti td) = (x==y) ||
                              if (x<y)
                                then belongBST x ti 
                                else belongBST x td

insertBST :: Ord a => a -> Tree a -> Tree a                   --Costo : O(log N), siendo N la cantidad total de elementos en el tree dado,
                                                              --ya que se recorre solamente una rama entera del árbol haciendo operaciones
                                                              --constantes, o sea, log n operaciones constantes. 
--Propósito : dado un BST inserta un elemento en el árbol.
--PRECOND: cumple los invariantes de BST y no tiene elementos repetidos.
--Observación= Para la medición de costos, asumimos que el árbol siempre se encuentra balanceado tras cada operación.
insertBST x EmptyT          = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if (x==y)
                                then (NodeT y ti td)
                                else if (x<y)
                                  then NodeT y (insertBST x ti) td 
                                  else NodeT y ti (insertBST x td)

deleteBST :: Ord a => a -> Tree a -> Tree a                   --Costo esperado : O(log N ) 
                                                              --Costo obtenido : O(log N+2*log n), siendo N la cantidad total
                                                              --de elementos en el tree dado y n la cantidad de elementos en el
                                                              --tree cuando se ejecuta newTree.
--Propósito : dado un BST borra un elemento en el árbol.
--PRECOND: cumple los invariantes de BST y no tiene elementos repetidos.
--Observación= Para la medición de costos, asumimos que el árbol siempre se encuentra balanceado tras cada operación.
deleteBST x EmptyT          = EmptyT
deleteBST x (NodeT y ti td) = if (x==y)
                                then newTree (NodeT y ti td) 
                                else if (x<y)
                                  then NodeT y (deleteBST x ti) td
                                  else NodeT y ti (deleteBST x td)                                                               

newTree :: Ord a => Tree a -> Tree a                          --Costo O(2*log n)
newTree (NodeT _ EmptyT td) = td
newTree (NodeT x ti td)     = let max = maximoIzq ti
                              in NodeT max (sinMaximo ti) td                                                  

maximoIzq :: Tree a -> a                                      --Costo O(log n), siendo n la cantidad total de elementos del tree dado.
maximoIzq (NodeT x _  EmptyT) = x
maximoIzq (NodeT _ ti td)     = maximoIzq td

sinMaximo :: Tree a -> Tree a                                 --Costo O(log n), siendo n la cantidad total de elementos del tree dado.
sinMaximo (NodeT x ti EmptyT) = ti
sinMaximo (NodeT x ti td)     = (NodeT x ti (sinMaximo td))

deleteBST' :: Ord a => a -> Tree a -> Tree a                   --Costo esperado : O(log N )
                                                               --Costo obtenido: en el peor caso es O(log N+log n), siendo N la 
                                                               --cantidad total de elementos en el tree dado y n la cantidad de 
                                                               --elementos que hay en ti cuando se ejecuta newTree'
--Propósito : dado un BST borra un elemento en el árbol.
--PRECOND: cumple los invariantes de BST y no tiene elementos repetidos.
--Observación= Para la medición de costos, asumimos que el árbol siempre se encuentra balanceado tras cada operación.
deleteBST' x EmptyT          = EmptyT
deleteBST' x (NodeT y ti td) = if (x==y)
                                then newTree' (NodeT y ti td)
                                else if (x<y)
                                  then NodeT y (deleteBST' x ti) td
                                  else NodeT y ti (deleteBST' x td)   

newTree' :: Ord a => Tree a -> Tree a                          --Costo O(log n)                              
newTree' (NodeT _ EmptyT td) = td
newTree' (NodeT x ti td)     = let (max, sinMax) = splitMaxBST ti
                               in NodeT max sinMax td

splitMinBST :: Ord a => Tree a -> (a, Tree a)                  --Costo O(log N), siendo la cantidad total de elementos en el tree dado.
                                                               --Es log N porque siempre recorre una rama entera de un tree balanceado,
                                                               --de modo que hace log N operaciones en todos los casos.
--Propósito : dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
--PRECOND: cumple los invariantes de BST y no tiene elementos repetidos.
--Observación= Para la medición de costos, asumimos que el árbol siempre se encuentra balanceado tras cada operación.
splitMinBST (NodeT x EmptyT td) = (x, td)
splitMinBST (NodeT x ti td)     = let (min, ti') = splitMinBST ti
                                  in (min, NodeT x ti' td)                               

splitMaxBST :: Ord a => Tree a -> (a, Tree a)                  --Costo O(log N), siendo la cantidad total de elementos en el tree dado.
                                                               --Es log N porque siempre recorre una rama entera de un tree balanceado,
                                                               --de modo que hace log N operaciones en todos los casos.
--Propósito : dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
--PRECOND: cumple los invariantes de BST y no tiene elementos repetidos.
--Observación= Para la medición de costos, asumimos que el árbol siempre se encuentra balanceado tras cada operación.
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST (NodeT x ti td)     = let (max, td') = splitMaxBST td
                                  in (max, (NodeT x ti td'))                                 

esBST :: Ord a => Tree a -> Bool                              --Costo: O(N*N), siendo N la cantidad total de elementos en el tree dado,
                                                              --ya que se ejecutan N operaciones donde se llevan a cabo 2 operaciones que
                                                              --son ambas lineales en aproximádamente la mitad de N, por lo que terminan 
                                                              --siendo equivalentes a una operación lineal en N
--Propósito: indica si el árbol cumple con los invariantes de BST.
esBST EmptyT          = True
esBST (NodeT x ti td) = todosMenores x ti && todosMayores x td && esBST ti && esBST td

todosMenores :: Ord a => a -> Tree a -> Bool                  --Costo O(n), siendo la cantidad de elementos en el tree dado
todosMenores x EmptyT          = True
todosMenores x (NodeT y ti td) = x>y && todosMenores x ti && todosMenores x td

todosMayores :: Ord a => a -> Tree a -> Bool                  --Costo O(n), siendo la cantidad de elementos en el tree dado
todosMayores x EmptyT          = True
todosMayores x (NodeT y ti td) = x<y && todosMayores x ti && todosMayores x td                                                                  



elMaximoMenorOIgualA :: Ord a => a -> Tree a -> Maybe a    --Costo: O(log N), siendo N la cantidad de elementos en el tree dado. Esto es
                                                           --así porque en el peor de los casos se recorre toda una rama entera, y, como
                                                           --el árbol está balanceado, el costo será log N.
--PRECOND= El tree dado es un BST.
--Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor o igual al elemento dado.
elMaximoMenorOIgualA x EmptyT          = Nothing
elMaximoMenorOIgualA x (NodeT y ti td) = if (x==y)
                                           then Just x
                                           else if (x>y)
                                             then maxEntre y (elMaximoMenorOIgualA x td)
                                             else elMaximoMenorOIgualA x ti

maxEntre :: a -> Maybe a -> Maybe a
maxEntre e Nothing = Just e
maxEntre _ m       = m

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a    --Costo: O(log N), siendo N la cantidad de elementos en el tree dado. Esto es
                                                           --así porque en el peor de los casos se recorre toda una rama entera, y, como
                                                           --el árbol está balanceado, el costo será log N.
--PRECOND= El tree dado es un BST.
--Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
elMaximoMenorA x EmptyT          = Nothing
elMaximoMenorA x (NodeT y ti td) = if (x>y)
                                     then maxEntre y (elMaximoMenorA x td)
                                     else elMaximoMenorA x ti

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a          --Costo: O(log N), siendo N la cantidad de elementos en el tree dado. Esto es
                                                           --así porque en el peor de los casos se recorre toda una rama entera, y, como
                                                           --el árbol está balanceado, el costo será log N.
--Propósito : dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado
elMinimoMayorA x EmptyT          = Nothing
elMinimoMayorA x (NodeT y ti td) = if (x<y)
                                     then minEntre y (elMinimoMayorA x ti)
                                     else elMinimoMayorA x td

minEntre :: a -> Maybe a -> Maybe a 
minEntre e Nothing = Just e
minEntre _ e       = e

balanceado :: Ord a => Tree a -> Bool                               --Costo : O(N*N), siendo N la cantidad de elementos en el tree dado.
                                                                    --Esto es así porque se realiza N veces la operación lineal sobre N 
                                                                    --hayBalance
--Propósito : indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
--nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
balanceado EmptyT          = True
balanceado (NodeT x ti td) = hayBalance ti td && balanceado ti && balanceado td 

hayBalance :: Ord a => Tree a -> Tree a -> Bool                    --Costo : O(n+m), siendo n la cantidad de elementos en el primer tree
                                                                   --dado y m la cantidad en el segundo tree dado.
hayBalance ti td = let (hti, htd) = (heightT ti, heightT td)
                   in ( (hti - htd) == 0 || (hti - htd) == 1 ) || ( (htd - hti) == 0 || (htd - hti) == 1 )

heightT :: Tree a -> Int                                           --Costo : O(n), siendo n la cantidad de elementos en el tree dado.
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + mayorEntre (heightT t1) (heightT t2)

mayorEntre :: Int -> Int -> Int                                    --Costo : O(1)
mayorEntre n1 n2 = if (n1 > n2)
                        then n1
                        else n2 

--EJERCICIO 3

{-

INTERFAZ DE MAP :
emptyM :: Map k v
Costo: O(1).

assocM :: Ord k => k -> v -> Map k v -> Map k v
Costo: O(log K).

lookupM :: Ord k => k -> Map k v -> Maybe v
Costo: O(log K).

deleteM :: Ord k => k -> Map k v -> Map k v
Costo: O(log K).

keys :: Map k v -> [k]
Costo: O(K).

==================================================

*K es la cantidad de claves del Map (y, por ende, de asociaciones)

valuesM :: Eq k => Map k v -> [Maybe v]                      --Costo O(K*log K+K), siendo K la cantidad de elementos del map dado.
                                                             --valuesL es de costo O(K*log x), pero siendo sus dos parámetros de largo K,
                                                             --es de costo O(K*log K). A esto se le suma el costo O(K) de keys.
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = valuesL (keys m) m

valuesL ::  Eq k => [k] -> Map k v -> [Maybe v]              --Costo O(K*log K), siendo K la cantidad de elementos de la lista dada y del
                                                             --map dado -si fueran distintos, sería O(K*log x)-
valuesL []     _ = []
valuesL (k:ks) m = lookupM k m : valuesL ks m

todasAsociadas :: (Eq k, Eq v) => [k] -> Map k v -> Bool     --Costo O(n*log K), siendo n la cantidad de elementos de la lista dada y K la
                                                             --cantidad de elementos del map dado. LookupM es logarítmica en K, y se 
                                                             --realiza n veces.
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas []     _ = True
todasAsociadas (k:ks) m = let asociada = lookupM k m /= Nothing
                          in asociada && todasAsociadas ks m 

listToMap :: Eq k => [(k, v)] -> Map k v                     --Costo O(n*(log n)), siendo n la cantidad de elementos en la lista dada. Esto
                                                             --es así porque se realiza n veces assocM, que es logarítmica sobre n.
--Propósito: convierte una lista de pares clave valor en un map.
listToMap []     = emptyM
listToMap (x:xs) = assocM (fst(x)) (snd(x)) (listToMap xs)  

mapToList :: Eq k => Map k v -> [(k, v)]                     --Costo O(K*log K+K) (K es el costo de keys)
mapToList m = armarListaTuplas (keys m) m  

armarListaTuplas :: Eq k => [k] -> Map k v -> [(k, v)]       --Costo O(K*log n, que, como son iguales, es K*log K)
armarListaTuplas []     _ = []
armarListaTuplas (k:ks) m = let v = fromJust (lookupM k m)
                            in (k, v) : armarListaTuplas ks m   

agruparEq :: Eq k => [(k, v)] -> Map k [v]              --COSTO O(k*K+log k+K*K), siendo K la cantidad de elementos de la lista de tuplas 
                                                        --dada y k la cantidad de claves sin repetir de la misma. En valoresDeClaves se 
                                                        --realizan k*K+log k operaciones constantes y en sinRepes se realizan K*K.
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--la misma clave.    
agruparEq xs = valoresDeClaves (sinRepes xs) xs 

valoresDeClaves :: Eq k => [k] -> [(k, v)] -> Map k [v] --COSTO O(k*K+log k), siendo k la cantidad de elementos de la lista de claves dada
                                                        --y K la cantidad de elementos de la lista de tuplas dada. Es k*K ya que se
                                                        --realizan k operaciones donde, a su vez, se realizan K operaciones constantes
                                                        -- -valoresDeClave- y luego otras log k -assocM sobre la parte recursiva-                                                        --
valoresDeClaves []     _  = emptyM
valoresDeClaves (x:xs) ys = let vs = valoresDeClave x ys
                            in assocM x vs (valoresDeClaves xs ys)              

valoresDeClave :: Eq k => k -> [(k,v)] -> [v]           --COSTO O(K), siendo K la cantidad de elementos de la lista de tuplas dada.
valoresDeClave _ []     = []
valoresDeClave x (y:ys) = if (x==(fst(y)))
                            then snd(y) : valoresDeClave x ys
                            else valoresDeClave x ys           

sinRepes :: Eq k => [(k,v)] -> [k]                      --COSTO O(K*K), siendo K la cantidad de elementos presentes en la lista de
                                                        --tuplas dada. Es K*K porque se realizan K operaciones lineales sobre K -estaRepe-
sinRepes []     = []
sinRepes (x:xs) = if (estaRepe (fst(x)) xs)
                    then sinRepes xs
                    else fst(x) : sinRepes xs

estaRepe :: Eq k => k -> [(k,v)] -> Bool                --COSTO O(K), siendo K la cantidad de elementos presentes en la lista dada.
estaRepe x []     = False
estaRepe x (y:ys) = x==fst(y) || estaRepe x ys  

incrementar :: Eq k => [k] -> Map k Int -> Map k Int         --Costo O(k*(log K*2)), siendo k la cantidad de elementos presentes en la lista
                                                             --dada y K la cantidad de elementos presentes en el map dado. Se ejecuta k
                                                             --veces incrementarSi, que tiene costo log K*2.
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--cada número asociado con dichas claves. 
incrementar []     m = m
incrementar (x:xs) m = incrementarSi x $ incrementar xs m

incrementarSi :: Eq k => k -> Map k Int -> Map k Int         --Costo O(log K*2), siendo K la cantidad de elementos presentes en el map dado.
                                                             --Esto es así porque se ejecutan 2 operaciones logarítmicas sobre K -lookupM y
                                                             --assocM-
incrementarSi x m = case (lookupM x m) of
                      Nothing -> m
                      Just n  -> assocM x (n+1) m

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v            --Costo O(k*(log K+log K')+K), siendo k la cantidad de claves del primer map 
                                                             --dado, K la cantidad de elementos de este mismo y K' la cantidad de elementos
                                                             --del segundo map dado. Esto es así porque se realizan k operaciones
                                                             --logarítmicas sobre K -lookupM- y k operaciones logarítmicas sobre K' -assocM-,
                                                             --además de una operación lineal sobre K -keys-.
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps m1 m2 = mergeMaps' (keys m1) m1 m2 

mergeMaps' :: Eq k => [k] -> Map k v -> Map k v -> Map k v   --Costo O(k*(log K+log K')), siendo k la cantidad de elementos de la lista dada,
                                                             --K la cantidad de elementos del primer map dado y K' la cantidad de elementos
                                                             --del segundo map dado. Esto es así porque se realizan k operaciones
                                                             --logarítmicas sobre K -lookupM- y k operaciones logarítmicas sobre K' -assocM-
mergeMaps' []     _  m2 = m2
mergeMaps' (k:ks) m1 m2 = let v = fromJust (lookupM k m1)
                          in mergeMaps' ks m1 (assocM k v m2)                      

-}


--DEFINICIONES AUXILIARES =

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
  deriving Show

inorderT :: Tree a -> [a]
inorderT EmptyT          = []
inorderT (NodeT x ti td) = (inorderT ti) ++ [x] ++ (inorderT td)  