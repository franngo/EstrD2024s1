--TADS de los que somos usuarios:

Sector, siendo C la cantidad de componentes y T la cantidad de tripulantes:
crearS :: SectorId -> Sector O(1)
sectorId :: Sector -> SectorId O(1)
componentesS :: Sector -> [Componente] O(1)
tripulantesS :: Sector -> Set Nombre O(1)
agregarC :: Componente -> Sector -> Sector O(1)
agregarT :: Nombre -> Sector -> Sector O(log T)

Tripulante, siendo S la cantidad de sectores:
crearT :: Nombre -> Rango -> Tripulante O(1)
asignarS :: SectorId -> Tripulante -> Tripulante O(log S)
sectoresT :: Tripulante -> Set SectorId O(1)
nombre :: Tripulante -> String O(1)
rango :: Tripulante -> Rango

Set, siendo N la cantidad de elementos del conjunto:
emptyS :: Set a O(1)
addS :: a -> Set a -> Set a O(log N )
belongsS :: a -> Set a -> Bool O(log N )
unionS :: Set a -> Set a -> Set a O(N log N )
setToList :: Set a -> [a] O(N )
sizeS :: Set a -> Int O(1)

MaxHeap, siendo M la cantidad de elementos en la heap:  
emptyH :: MaxHeap a O(1)
isEmptyH :: MaxHeap a -> Bool O(1)
insertH :: a -> MaxHeap a -> MaxHeap a O(log M )
maxH :: MaxHeap a -> a O(1)
deleteMaxH :: MaxHeap a -> MaxHeap a O(log M )

Map, siendo K la cantidad de claves distintas en el map:
emptyM :: Map k v O(1)
assocM :: k -> v -> Map k v -> Map k v O(log K)
lookupM :: k -> Map k v -> Maybe v O(log K)
deleteM :: k -> Map k v -> Map k v O(log K)
domM :: Map k v -> [k] O(K)

--Otros tipos usados:

type SectorId = String

type Nombre = String

type Rango = String

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]

data Barril = Comida | Oxigeno | Torpedo | Combustible

=========================================================================================================================================

--TAD a definir:

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
INV.REP.: 
    *Sea N mss mnt mht
    *Si un tripulante pertenece como valor a un sector de mss, existe también como valor en mnt
    *Si un tripulante pertenece como valor a un sector de mss, existe también como valor en mht
    *Todos los tripulantes que pertenecen como valor a mnt, pertenecen también como valor a mht
    *Todos los tripulantes que pertenecen como valor a mht, pertenecen también como valor a mnt
    *En cada asociación de mss, el sectorid tiene que coincidir con el que posee como valor 
    su sector relacionado
    *En cada asociación de mnt, el nombre tiene que coincidir con el que posee como valor 
    su tripulante relacionado
    *Si un sector aparece como valor de un tripulante en mnt, este tiene que aparecer como valor 
    en mss.

-lo pedido era escribir el invariante para nave-    

LOS INVARIANTES DE UNA NAVE NOS DICEN SI UN DATO DE TIPO NAVE ES VÁLIDO -hace de cuenta que los datos de
otros tipos cumplen sus invariantes y son correctos-
SOLO SE COLOCAN LAS COSAS QUE HACEN INVALIDA A UNA NAVE Y QUE DEBEN EVITARSE PARA CONSTRUIR UNA NAVE VÁLIDA

NO son las cosas que lo hacen no funcional (para eso ya está la lógica de haskell) o que hacen a ciertas estructuras que lo conforman
inválidas (x ej: en la parte implementada con max heap, el máximo elemento del tree debe estar en la raíz) (para eso están los invariantes
de representación de los distintos tipos), SINO las cosas que lo hacen inválido como una nave (esto en base a QUÉ REPRESENTA UNA NAVE,
que lo deducimos de lo que nos dice el enunciado).

--orden de resolución ejs recomendado= implementar como user - escribir inv rep - implementar como implementador

=========================================================================================================================================

--FUNCIONES IMPLEMENTADAS COMO IMPLEMENTADOR DE Nave (operaciones de la interfaz):

definicion del tipo:
data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

construir :: [SectorId] -> Nave                                                        --PROBLEMA DE COSTO
Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
Eficiencia: O(S)
Costo Obtenido O(S log S)
construir sis = let msis = sectoresNuevos sis
                in N msis emptyM emptyH 

sectoresNuevos :: [SectorId] -> Map SectorId Sector
Costo O(S log S)
sectoresNuevos []     = emptyM
sectoresNuevos (s:ss) = assocM s (crearS s) sectoresNuevos ss     

ingresarT :: Nombre -> Rango -> Nave -> Nave
Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
Eficiencia: O(log T ), siendo T la totalidad de los tripulantes de la nave. Esto es así porque crearT y nombre son constantes, mientras
que, tanto assocM como insertH, son logarítmicas sobre T, dejándonos así un costo final de O(log T).
ingresarT n r (N msis mnt mht) = let t = crearT n r
                                 in N msis (assocM (nombre t) t mnt) (insertH t mht)

sectoresAsignados :: Nombre -> Nave -> Set SectorId
Propósito: Devuelve los sectores asignados a un tripulante.
Precondición: Existe un tripulante con dicho nombre.
Eficiencia: O(log T ), siendo T la totalidad de tripulantes de la nave dada. Esto es así porque lookupM es una operación logarítmica sobre
T, mientras que sectoresT tiene costo constante, dando así el costo final O(log T).
sectoresAsignados n (N msis mnt mht) = case (lookupM n mnt) of
                                         Nothing -> error "El tripulante dado no está en la nave"
                                         Just t  -> sectoresT t   

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
Precondición: Existe un sector con dicho id.
Eficiencia: O(log S), siendo S la totalidad de sectores de la nave. Esto es así porque lookupM es una operación logarítmica sobre S, 
mientras que tripulantesS y componentesS son ambas de costo constante, por lo que el costo final es O(log S).
datosDeSector si (N msis mnt mht) = let s = lookupM si msis
                                    in ((tripulantesS s), (componentesS s))

tripulantesN :: Nave -> [Tripulante]                                                   --PROBLEMA DE COSTO
Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
Eficiencia: O(log T )
tripulantesN (N msis mnt mht) = tripulantesN' mht

tripulantesN' :: MaxHeap Tripulante -> [Tripulante] 
Costo O(T * log T + T) -distinto de O(log T)-
tripulantesN' mhp = if (isEmptyH)
                      then []
                      else let max = maxH mht
                           in [max] ++ (tripulantesN' deleteMaxH mhp)

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
Propósito: Asigna una lista de componentes a un sector de la nave.
Eficiencia: O(C + log S), siendo C la cantidad de componentes dados y S la totalidad de sectores de la nave dada. Esto es así porque se
ejecuta una operación de costo O(C), como lo es agregarComponentes, además de dos operaciones logarítmicas sobre S, que son lookupM y 
assocM, dejándonos como costo final O(C + log S).
agregarASector cs si (N msis mnt mht) = case (lookupM si msis) of
                                          Nothing -> error "El sector dado no existe dentro de la nave"
                                          Just s  -> N (assocM si (agregarComponentes cs s) msis) mnt mht

agregarComponentes :: [Componente] -> Sector -> Sector 
Costo O(C), siendo C la totalidad de los componentes dados. Esto es así porque se realizan C operaciones donde se ejecuta agregarC, la cual
es una operación de costo constante, dando como costo final O(C).
agregarComponentes []     s = s
agregarComponentes (c:cs) s = agregarC c (agregarComponentes cs s)

asignarASector :: Nombre -> SectorId -> Nave -> Nave
Propósito: Asigna un sector a un tripulante.
Nota: No importa si el tripulante ya tiene asignado dicho sector.
Precondición: El tripulante y el sector existen.
Eficiencia: O(log S + log T + T log T ), siendo S la totalidad de los sectores de la nave y T la totalidad de los tripulantes de la misma.
Esto es así porque -lookupM si msis-, -assocM si s msis- y asignarS son operaciones con costo logarítmico sobre S, -lookupM n mnt-, 
-assocM n t' mnt- y agregarT son operaciones con costo logarítmico sobre T y modificarH es una operacion con costo T log T. Por consiguiente,
el costo final de esta operación es de O(log S + log T + T log T).
asignarASector n si (N msis mnt mht) = case (lookupM n mnt) of
                                         Nothing -> error "Este tripulante no pertenece a la nave dada"
                                         Just t  -> let t' = asignarS si t 
                                                         s = agregarT n (lookupM si msis)
                                                    in N (assocM si s msis) (assocM n t' mnt) (modificarH t' mht)

modificarH :: Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante 
Costo O(T * log T), siendo T la totalidad de los tripulantes de la estructura dada. Esto es así porque, en el peor caso, se realizan T 
operaciones donde, en cada una de ellas, se ejecutan 2 operaciones logarítmicas sobre T, como lo son insertH y deleteMaxH, además de
las operaciones constantes maxH y nombre. Es por esto que el costo final de la función es de O(T * log T)
modificarH t mht = let max = maxH mht 
                   in if((nombre t)==(nombre max))
                        then insertH t (deleteMaxH mht) 
                        else insertH max (modificarH t (deleteMaxH mht))
                                                                  

=========================================================================================================================================

--FUNCIONES IMPLEMENTADAS COMO USUARIO DE Nave:

Interfaz del TAD Nave:

construir :: [SectorId] -> Nave
Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
Eficiencia: O(S)

ingresarT :: Nombre -> Rango -> Nave -> Nave
Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
Eficiencia: O(log T )

sectoresAsignados :: Nombre -> Nave -> Set SectorId
Propósito: Devuelve los sectores asignados a un tripulante.
Precondición: Existe un tripulante con dicho nombre.
Eficiencia: O(log M )

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
Precondición: Existe un sector con dicho id.
Eficiencia: O(log S)

tripulantesN :: Nave -> [Tripulante]
Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
Eficiencia: O(log T )

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
Propósito: Asigna una lista de componentes a un sector de la nave.
Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.

asignarASector :: Nombre -> SectorId -> Nave -> Nave
Propósito: Asigna un sector a un tripulante.
Nota: No importa si el tripulante ya tiene asignado dicho sector.
Precondición: El tripulante y el sector existen.
Eficiencia: O(log S + log T + T log T )

--FUNCIONES COMO USUARIO:

sectores :: Nave -> Set SectorId
Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
Costo O(log T + T * (S log S)), siendo T la cantidad total de tripulantes de toda la nave y siendo S la cantidad total de sectores de toda
la nave. Esto es así porque tripulantesN tiene costo logarítmico sobre T y sectores' tiene costo T * (S log S), ya que en esta, en el peor de
los casos, se realizan T operaciones donde, en cada una de ellas, se ejecuta sectoresT de costo constante y unionS de costo "eneloguéne"
sobre S.
sectores nave = sectores' (tripulantesN nave)

sectores' :: [Tripulante] -> Set SectorId
sectores' []     = emptyS
sectores' (x:xs) = let s = sectoresT x
                   in unionS s (sectores' xs)

sinSectoresAsignados :: Nave -> [Tripulante]
Propósito: Devuelve los tripulantes que no poseen sectores asignados.
Costo O(log T + T), siendo T la cantidad de tripulantes de la nave. Esto es así porque TripulantesN tiene costo logarítmico sobre T y 
sinSectoresAsignados' tiene costo lineal sobre T, ya que en esta se recorre la totalidad de los tripulantes, por lo que se realizan T 
operaciones donde, en cada una de estas, se ejecutan sizeS, sectoresT y una igualdad entre ints, que son todas operaciones constantes. 
sinSectoresAsignados nave = sinSectoresAsignados' (tripulantesN nave)

sinSectoresAsignados' :: [Tripulante] -> [Tripulante]
sinSectoresAsignados' []     = []
sinSectoresAsignados' (t:ts) = if (sizeS (sectoresT t) == 0)
                                 then t : sinSectoresAsignados' ts
                                 else sinSectoresAsignados' ts

barriles :: Nave -> [Barril]                                                     --PROBLEMA DE COSTO
Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
Costo O( (log T + T * (S log S)) + s + (s * (c * (b * bs) ) + bs * B) )
barriles nave = barriles' (setToList (sectores nave)) nave 

barriles' :: [SectorId] -> Nave -> [Barril]
Costo O(s * (c * (b * bs) ) + bs * B), siendo s la cantidad de sectores con personas asignadas de la nave, c la cantidad de componentes
de cada sector de s, b la cantidad de barriles en cada componente de cada sector de s, bs la cantidad de barriles en cada sector de s y
B la totalidad de barriles de la nave.
barriles' []     = []
barriles' (x:xs) = let (_,cs) = datosDeSector x nave
                   in barrilesDe cs ++ barriles' xs

barrilesDe :: [Componente] -> [Barril]
Costo O(c * (b * bs) ), siendo c la cantidad de componentes de la lista dada, b la cantidad de barriles de cada componente y bs el total
de barriles en toda la lista. 
barrilesDe []     = []
barrilesDe (x:xs) = case (x) of
                      Almacen bs -> bs ++ barrilesDe xs
                      _          -> barrilesDe xs       

=========================================================================================================================================

Bonus
-Dar una posible representación para el tipo Sector, de manera de que se pueda cumplir con el orden dado para cada
operación de la interfaz, pero sin implementarlas.

data Sector = S SectorId [Componente] (Set Nombre) //NO Set Tripulante
