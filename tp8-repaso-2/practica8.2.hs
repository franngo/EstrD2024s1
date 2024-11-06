data RAList a = MkR Int (Map Int a) (Heap a)

Interfaces

La interfaz de Heap, siendo H la cantidad de elementos en la heap:
emptyH :: Heap a
O(1)
isEmptyH :: Heap a -> Bool
O(1)
findMin :: Heap a -> a
O(1)
insertH :: Ord a => a -> Heap a -> Heap a O(log H)
deleteMin :: Ord a => Heap a -> Heap a
O(log H)

La interfaz de Map, siendo K la cantidad de claves distintas en el map:
emptyM :: Map k v
O(1)
assocM :: Ord k => k -> v -> Map k v -> Map k v O(log K)
lookupM :: Ord k => Map k v -> k -> Maybe v
O(log K)
deleteM :: Ord k => k ->Map k v -> Map k v
O(log K)
domM :: Map k v -> [k]
O(K)

========================================================================================================================================

INVARIANTE DE REPRESENTACIÓN:

data RAList a = MkR Int (Map Int a) (Heap a)
{-INV. REP.:
    *Sea MkR n mna ha
    *En mna debe haber una cantidad de pares equivalente a n.
    *Ni n ni ningún valor mayor a n pueden existir como claves en algún par de mna.
    *Ningún número negativo puede existir como clave en algún par de mna.
    *La cantidad de elementos en ha debe ser igual a la cantidad de elementos en mna.
    *Todos los valores de los pares de mna deben existir como elementos de ha.
    *Todos los elementos de ha deben existir como valores en algún par de mna.
}

========================================================================================================================================

OPERACIONES IMPLEMENTADAS COMO IMPLEMENTADOR (INTERFAZ) =

emptyRAL :: RAList a
Propósito: devuelve una lista vacía.
Eficiencia: O(1), ya que se ejecutan emptyM y emptyH, que son ambas operaciones constantes.
emptyRAL = MkR 0 emptyM emptyH

isEmptyRAL :: RAList a -> Bool
Propósito: indica si la lista está vacía.
Eficiencia: O(1), ya que solamente se ejecuta una operación de igualdad entre 2 datos de tipo de Int, cuya comparación es constante.
isEmptyRAL (MkR n mna ha) = n==0

lengthRAL :: RAList a -> Int
Propósito: devuelve la cantidad de elementos.
Eficiencia: O(1), ya que no se realizan operaciones más alla del pattern matching y la operación de asignación = (las cuales tienen costo
constante).
lengthRAL (MkR n mna ha) = n

get :: Int -> RAList a -> a
Propósito: devuelve el elemento en el índice dado.
Precondición: el índice debe existir.
Eficiencia: O(log N ), siendo N la totalidad de elementos en la RA list dada. Esto es así porque lookupM es una operación de costo
logarítmico sobre N.
get n (MkR n mna ha) = case (lookupM n mna) of
                         Nothing -> error "No hay un elemento con ese índice en la RA list dada"
                         Just e  -> e 

minRAL :: Ord a => RAList a -> a
Propósito: devuelve el mínimo elemento de la lista.
Precondición: la lista no está vacía.
Eficiencia: O(1), ya que solo se ejecuta findMin, que es una operación de costo constante, por lo que el costo final es O(1).
minRAL (MkR n mna ha) = findMin ha 

add :: Ord a => a -> RAList a -> RAList a
Propósito: agrega un elemento al final de la lista.
Eficiencia: O(log N ), siendo N la totalidad de elementos de la RA list dada. Esto es así porque tanto assocM como insertH son operaciones 
de costo logarítmico sobre N, lo que nos deja un costo final de O(log N).
add x (MkR n mna ha) = let n' = n+1
                       in (MkR n' (assocM n' x mna) (insertH x ha))

elems :: Ord a => RAList a -> [a]
Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
Eficiencia: O(N log N ), siendo N la totalidad de elementos de la RA list dada. Esto es así porque se ejecuta la función elems', la cual
tiene un costo de N * log N, por lo que el costo final no es otro que O(N log N).
elems (MkR n mna ha) = elems' 0 n mna 

elems' :: Int -> Int -> Map Int a -> [a]
Costo O(N log N), siendo N la totalidad de elementos del map dado. Esto es así porque se realizan N operaciones donde, en cada una de ellas,
se ejecuta lookupM, la cual es una operación de costo logarítmico sobre N, además de fromJust, que tiene costo constante. Es por esto que el
costo final es O(N log N).
elems' n tope mna = let elem = fromJust (lookupM n mna)
                    in if (n==tope)
                         then []
                         else elem : elems' (n+1) tope mna

remove :: Ord a => RAList a -> RAList a
Propósito: elimina el último elemento de la lista.
Precondición: la lista no está vacía.
Eficiencia: O(N log N ), siendo N la totalidad de elementos de la RA list dada. Esto es así porque deleteH tiene un costo "eneloguéne" sobre
N, o sea, O(N log N), y tanto lookupM como deleteM tienen un costo logarítmico sobre N, lo que nos deja un costo final de O(N log N).
remove (MkR n mna ha) = let n'= n-1
                            elem = fromJust (lookupM n' mna)
                        in MkR n' (deleteM n' mna) (deleteH elem ha)

deleteH :: a -> Heap a -> Heap a
Costo O(N log N), siendo N la totalidad de elementos del heap dado. Esto es así porque se realizan, en el peor de los casos, N operaciones 
donde, en cada una de ellas, se ejecuta insertH y deleteMin, las cuales son ambas operaciones de costo logarítmico sobre N, además de 
findMin y un ==, que son operaciones de costo constante. Es por esto que el costo final es O(N log N).
deleteH elem ha = let min = findMin ha
                  in if (elem==min)
                       then deleteMin ha
                       else insertH min (deleteH elem (deleteMin ha))

set :: Ord a => Int -> a -> RAList a -> RAList a
Propósito: reemplaza el elemento en la posición dada.
Precondición: el índice debe existir.
Eficiencia: O(N log N ), siendo N la totalidad de elementos en la RA list dada. Esto es así porque deleteH tiene un costo "eneloguéne"
sobre N, o sea, O(N log N), además de que lookupM, assocM e insertH tienen un costo logarítmico sobre N y fromJust tiene un costo constante.
Es por esto que el costo final de la operación es de O(N log N).
set n' newE (MkR n mna ha) = let oldE = fromJust (lookupM n' mna)
                             in MkR n (assocM n' newE mna) (insertH newE (deleteH oldE ha))

addAt :: Ord a => Int -> a -> RAList a -> RAList a
Propósito: agrega un elemento en la posición dada.
Precondición: el índice debe estar entre 0 y la longitud de la lista.
Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
Eficiencia: O(N log N ), siendo N la totalidad de elementos de la RA list dada. Esto es así porque se ejecuta moveFromTo, que es una 
operación de costo "eneloguéne" sobre N, o sea, O(N log N), además de las operaciones de costo logarítmico sobre N assocM e insertarH. 
Es por esto que el costo final de la operación es O(N log N).
Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar
también como argumento la máxima posición posible.
addAt n' elem (MkR n mna ha) = (MkR (n+1) (assocM n' elem (moveFromTo n' (n-1) mna)) (insertarH elem ha))

moveFromTo :: Int -> A -> Map Int a -> Map Int a 
Costo O(N log N), siendo N la totalidad de elementos del map dado. Esto es así porque se realizan, en el peor de los casos, N operaciones 
donde, en cada una de ellas, se ejecuta lookupM y assocM, las cuales son operaciones logarítmicas sobre N, a las cuales se suma la operación
constante fromJust. Es por esto que el costo final de la función es O(N log N).
moveFromTo end n mna = let elem = fromJust (lookupM n mna)
                       in if (n==end)
                            then assocM (n+1) elem mna
                            else moveFromTo end (n-1) (assocM (n+1) elem mna)
