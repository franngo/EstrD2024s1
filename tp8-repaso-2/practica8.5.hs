INTERFACES=

La interfaz de Map, siendo N la cantidad de claves distintas en el map:
emptyM :: Map k v
assocM :: k -> v -> Map k v -> Map k v
lookupM :: Map k v -> k -> Maybe v
removeM :: Map k v -> k -> Map k v
domM :: Map k v -> [k]

La interfaz de Set, siendo N la cantidad de elementos del conjunto:
emptyS :: Set a
addS :: a -> Set a -> Set a
removeS :: a -> Set a -> Set a
belongs :: a -> Set a -> Bool
union :: Set a -> Set a -> Set a
intersection :: Set a -> Set a -> Set a
set2list :: Set a -> [a]
sizeS :: Set a -> Int

La interfaz de Heap, siendo N la cantidad de elementos de la heap:
emptyH :: Heap a
isEmptyH :: Heap a -> Bool
insertH :: a -> Heap a -> Heap a
findMin :: Heap a -> a
deleteMin :: Heap a -> Heap a
splitMin :: Heap a -> (a, Heap a)

Los tipos Tripulante, Rango y Sector son abstractos. Pero sabemos que son comparables (Ord, Eq) y que el tipo Tripulante
posee esta función en su interfaz (la única que nos interesa):
rango :: Tripulante -> Rango

=======================================================================================================================================

INVARIANTE DE REPRESENTACIÓN= 

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
{-INV. REP.:
 *Sea MkN msst ht smax
 *Todos los tripulantes de los sets asociados a sectores en msst pertenecen también a ht
 *Los tripulantes de cada set relacionado a un sector en msst sumados no pueden ser más que la cantidad de tripulantes en ht.
 *No puede haber un mismo tripulante en 2 sets relacionados a un sector en msst diferentes.
 *La clave de smax debe equivaler al sector que está relacionado con el set con la mayor cantidad de tripulantes en msst, y el valor de 
 smax debe equivaler a dicha cantidad de tripulantes.
 *msst no puede ser vacío. Debe existir al menos una asociación en este.
-}

=======================================================================================================================================

operaciones como implementador =

(varias las salteé porque eran más de lo mismo)

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

naveVacia :: [Sector] -> Nave
Propósito: Crea una nave con todos esos sectores sin tripulantes.
Precondición: la lista de sectores no está vacía
Costo: O(S log S), siendo S la cantidad de sectores de la lista dada. Esto es así porque se ejecuta crearConSec, que es una operación 
de costo "eneloguéne" sobre S (o sea, S log S), además de las operaciones de costo constante emptyH y head.
naveVacia ss = MkN (crearConSec ss) emptyH ((head ss),0)

crearConSec :: [Sector] -> Map Sector (Set Tripulante)
Costo O(S log S), siendo S la cantidad de sectores de la lista dada. Esto es así porque se realiza S veces assocM, que es una operación
logarítmica sobre S, además de las operaciones de costo constante emptyM y emptyS.
crearConSec []     = emptyM
crearConSec (s:ss) = assocM s emptyS (crearConSec ss)

tripulantesDe :: Sector -> Nave -> Set Tripulante
Propósito: Obtiene los tripulantes de un sector.
Precondición: Debe existir el sector dado en la nave dada
Costo: O(log S) siendo S la cantidad de sectores de la nave dada. Esto es así porque se ejecuta lookupM, que es una operación con 
costo logarítmico sobre S.
tripulantesDe s (MkN msst ht smax) = case (lookupM s msst) of
                                       Nothing -> error "El sector dado no existe dentro de la nave dada"
                                       Just m  -> m 

sectores :: Nave -> [Sector]
Propósito: Denota los sectores de la nave
Costo: O(S) siendo S la cantidad de sectores de la nave dada. Esto es así porque se ejecuta domM, que es una operación de costo lineal 
sobre S.
sectores (MkN msst ht smax) = domM msst 

conMayorRango :: Nave -> Tripulante
Propósito: Denota el tripulante con mayor rango.
Precondición: la nave no está vacía.
Costo: O(1). Esto es así porque solamente se ejecuta la operación findMin, la cual tiene un costo constante.
conMayorRango (MkN msst ht smax) = findMin ht 

conMasTripulantes :: Nave -> Sector
Propósito: Denota el sector de la nave con más tripulantes.
Costo: O(1). Esto es así porque solamente se ejecuta la operación fst, la cual tiene un costo constante.
conMasTripulantes (MkN msst ht smax) = fst smax 

conRango :: Rango -> Nave -> Set Tripulante
Propósito: Denota el conjunto de tripulantes con dicho rango.
Costo: O(P log P ) siendo P la cantidad de tripulantes de la nave dada. Esto es así porque se ejecuta la operación conRango', que tiene 
un costo "eneloguéne" sobre P (o sea, P log P).
conRango r (MkN msst ht smax) = conRango' r ht 

conRango' :: Rango -> Heap Tripulante -> Set Tripulante
Costo O(P log P), siendo P la cantidad de tripulantes del heap dado. Esto es así porque se realizan P operaciones en las cuales, en el peor
de los casos, se ejecutan las operaciones addS y deleteMin, las cuales tienen un costo logarítmico sobre P, a las cuales se les suman otras
operaciones constantes como isEmptyH, findMin y rango.
conRango' r ht = if (isEmptyH ht)
                   then emptyS
                   else let t  = findMin ht
                            rt = rango t 
                        in if (rt==r)
                             then addS t (conRango' r (deleteMin ht))
                             else conRango' r (deleteMin ht)

sectorDe :: Tripulante -> Nave -> Sector
Propósito: Devuelve el sector en el que se encuentra un tripulante.
Precondición: el tripulante pertenece a la nave.
Costo: O(S log S + log P ) siendo S la totalidad de sectores de la nave dada y P la totalidad de tripulantes de la misma. Esto es así porque
se ejecuta la operación sectoresDe', que tiene un costo de O(S log S + log P), además de la operación lineal sobre S sectores.
sectorDe t nave = sectorDe' t (sectores nave)

sectorDe' :: Tripulante -> [Sector] -> Nave ->  Sector
Costo O(S log S + log P), siendo S la totalidad de sectores de la nave dada y P la totalidad de tripulantes de la misma. Esto es así porque,
en el peor de los casos, se realizan S operaciones en las cuales se ejecuta tripulantesDe, que tiene costo logarítmico sobre S, y belongs,
que tiene costo logarítmico sobre P.
sectorDe' t []     nave = error "El tripulante dado no pertenece a la nave dada"
sectorDe' t (s:ss) nave = let ts = tripulantesDe s nave
                          in if (belongs t ts)
                               then s
                               else sectorDe' t ss nave

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
Propósito: Agrega un tripulante a ese sector de la nave.
Precondición: El sector está en la nave y el tripulante no.
Costo: O((S log S) + (log P + log S)), siendo S la totalidad de sectores de la nave dada y P la totalidad de tripulantes de la misma. Esto 
es así porque se ejecuta la operación actualizarMax, que tiene un costo "eneloguéne" sobre S (o sea, S log S), además de agregarTripulante',
que tiene un costo de (log P + log S).
agregarTripulante t s nave = let nave' = agregarTripulante' t s nave
                             in actualizarMax nave'

agregarTripulante' :: Tripulante -> Sector -> Nave -> Nave
Costo O(log P + log S), siendo S la totalidad de sectores de la nave dada y P la totalidad de tripulantes de la misma. Esto es así porque se 
ejecuta tripuASector, que tiene un costo de O(log P + log S), además de tripulantesDe, que tiene costo logarítmico sobre S, e insertH, que 
tiene costo logarítmico sobre P.
agregarTripulante' t s (MkN msst ht smax) = let ts = tripulantesDe s (MkN msst ht smax)
                                            in (tripuASector t s ts msst) (insertH t ht) smax

tripuASector :: Tripulante -> Sector -> Set Tripulante -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
Precondición: El sector está en el map y el tripulante no.
Costo O(log P + log S), siendo S la totalidad de sectores del map dado y P la totalidad de tripulantes del mismo. Esto es así porque se
ejecuta la operación addS, que tiene costo logarítmico sobre P, y la operación assocM, que tiene costo logarítmico sobre S.
tripuASector t s ts msst = let newSet = addS t ts
                           in assocM s newSet msst

actualizarMax :: Nave -> Nave
Costo O(S log S), siendo S la totalidad de sectores de la nave dada y P la totalidad de tripulantes de la misma. Esto es así porque se 
ejecuta la operación actualizarMax', que tiene un costo "eneloguéne" sobre S (o sea, S log S), además de sectores, que tiene costo lineal 
sobre S.
actualizarMax (MkN msst ht smax) = let nave = (MkN msst ht smax)
                                   in (MkN msst ht (actualizarMax' smax (sectores nave) nave))

actualizarMax' :: (Sector, Int) -> [Sector] -> Nave -> (Sector, Int)
Costo O(S log S), siendo S la totalidad de sectores de la nave dada y P la totalidad de tripulantes de la misma. Esto es así porque
se realizan S operaciones donde se ejecuta tripulantesDe, que tiene costo logarítmico sobre S, además de sizeS y snd, que son operaciones 
constantes.
actualizarMax' smax []     nave = smax
actualizarMax' smax (s:ss) nave = let ts = tripulantesDe s nave
                                     lts = sizeS ts
                                     max = snd smax
                                 in if (lts>max)
                                      then actualizarMax' (s,lts) ss nave
                                      else actualizarMax' max ss nave
                   
                             
=======================================================================================================================================

operaciones como usuario =

INTERFAZ DE NAVE=

naveVacia :: [Sector] -> Nave
Propósito: Crea una nave con todos esos sectores sin tripulantes.
Precondición: la lista de sectores no está vacía
Costo: O(S log S) siendo S la cantidad de sectores de la lista.

tripulantesDe :: Sector -> Nave -> Set Tripulante
Propósito: Obtiene los tripulantes de un sector.
Costo: O(log S) siendo S la cantidad de sectores.

sectores :: Nave -> [Sector]
Propósito: Denota los sectores de la nave
Costo: O(S) siendo S la cantidad de sectores.

conMayorRango :: Nave -> Tripulante
Propósito: Denota el tripulante con mayor rango.
Precondición: la nave no está vacía.
Costo: O(1).

conMasTripulantes :: Nave -> Sector
Propósito: Denota el sector de la nave con más tripulantes.
Costo: O(1).

conRango :: Rango -> Nave -> Set Tripulante
Propósito: Denota el conjunto de tripulantes con dicho rango.
Costo: O(P log P ) siendo P la cantidad de tripulantes.

sectorDe :: Tripulante -> Nave -> Sector
Propósito: Devuelve el sector en el que se encuentra un tripulante.
Precondición: el tripulante pertenece a la nave.
Costo: O(S log S log P ) siendo S la cantidad de sectores y P la cantidad de tripulantes.

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
Propósito: Agrega un tripulante a ese sector de la nave.
Precondición: El sector está en la nave y el tripulante no.
Costo: O((S log S) + (log P + log S))

-OPERACIONES IMPLEMENTADAS COMO USUARIO:

tripulantes :: Nave -> Set Tripulante
Propósito: Denota los tripulantes de la nave
Costo: O(S (log S + P log P) + S), siendo S la totalidad de sectores de la nave dada y P la totalidad de tripulantes de la misma. Esto es
así porque se ejecuta la operación tripulantes', la cual tiene un costo de O(S (log S + P log P)), además de la operación sectores, que 
tiene un costo lineal sobre S.
tripulantes nave = let ss = sectores nave
                   in tripulantes' ss nave 

tripulantes' :: [Sector] -> Nave -> Set Tripulante 
Costo: O(S (log S + P log P)), siendo S la totalidad de sectores de la nave dada y P la totalidad de tripulantes de la misma. Esto es así 
porque se realizan S operaciones en las cuales se ejecuta tripulantesDe, que tiene un costo logarítmico sobre S, además de union, que tiene
un costo "eneloguéne" sobre P (o sea, P log P).
tripulantes' []     _    = emptyS
tripulantes' (s:ss) nave = let ts = tripulantesDe s nave
                           in union ts (tripulantes' ss nave)   

Opcional (Bonus): bajaDeTripulante :: Tripulante -> Nave -> Nave
Propósito: Elimina al tripulante de la nave.
Pista: Considere reconstruir la nave sin ese tripulante.
Costo O(S ( P ((s log s) + (log p + log s)) + log S + P) + S), siendo S la totalidad de sectores de la primer nave dada, P la totalidad de 
tripulantes de la misma, s la totalidad de sectores de la nave que se crea en la subtarea y p la totalidad de tripulantes de la misma.
Esto es así porque se ejecuta la operacion bajaDeTripulante', que tiene un costo de O(S ( P ((s log s) + (log p + log s)) + log S + P) ),
además de las operaciones lineales sobre S sectores y naveVacia.
Costo O() POR AHORA S -sectores- + S log S -naveVacia- + 
bajaDeTripulante t nave = let ss = sectores nave
                              nave' = naveVacia ss
                          in bajaDeTripulante' t ss nave nave'

bajaDeTripulante' :: Tripulante -> [Sector] -> Nave -> Nave -> Nave 
Costo O(S ( P ((s log s) + (log p + log s)) + log S + P) ), siendo S la totalidad de sectores de la primer nave dada, P la totalidad de 
tripulantes de la misma, s la totalidad de sectores de la segunda nave dada y p la totalidad de tripulantes de la misma. Esto es así
porque se realizan S operaciones en las cuales se ejecuta bajaDeTripulante', que es una operación de costo 
O(P ((s log s) + (log p + log s))), además de la operación logarítmica sobre S tripulantesDe y las operaciones lineales sobre P belongs 
y set2List.
bajaDeTripulante' _ []     _    newN = newN
bajaDeTripulante' t (s:ss) oldN newN = let ts = tripulantesDe s oldN
                                       in if (belongs t ts)
                                            then let ts'   = set2List (removeS t ts)
                                                     newN' = agregarVarios ts' s newN
                                                 in bajaDeTripulante' t ss oldN newN'
                                            else let ts'   = set2List ts
                                                     newN' = agregarVarios ts' s newN
                                                 in bajaDeTripulante' t ss oldN newN'

agregarVarios :: [Tripulante] -> Sector -> Nave -> Nave
Costo O(P ((s log s) + (log p + log s)) ), siendo s la totalidad de sectores de la nave dada, p la totalidad de tripulantes de la misma 
y P la totalidad de tripulantes de la nave de donde provienen los tripulantes dados. Esto es así porque se realizan, en el peor de los 
casos, P operaciones en las cuales se ejecuta la operación agregarTripulante, la cual tiene un costo de O((s log s) + (log p + log s)).
agregarVarios []     _ nave = nave
agregarVarios (t:ts) s nave = let nave'= (agregarTripulante t s nave)
                              in agregarVarios ts s nave'