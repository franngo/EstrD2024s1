--1. Pizzas

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa | Queso| Jamon | Aceitunas Int
    deriving Show

pizzaRica :: Pizza
pizzaRica = Capa Queso (Capa Salsa Prepizza)

pizzaDeliciosa :: Pizza
pizzaDeliciosa = Capa (Aceitunas 8) (Capa Queso (Capa Queso (Capa Jamon (Capa Queso Prepizza))))   

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 + (cantidadDeCapas p)

armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (x:xs) = (Capa x (armarPizza xs))

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if (esJamon i)
                          then (sacarJamon p)
                          else (Capa i (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = (esSalsaOQueso i)  && (tieneSoloSalsaYQueso p)

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _     = False

--VERSIÓN 1=

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (dobleAceitunasSi i) (duplicarAceitunas p)

dobleAceitunasSi :: Ingrediente -> Ingrediente
dobleAceitunasSi (Aceitunas n) = (Aceitunas (n*2))
dobleAceitunasSi i             = i

--VERSIÓN 2=

duplicarAceitunas' :: Pizza -> Pizza
duplicarAceitunas' Prepizza   = Prepizza
duplicarAceitunas' (Capa i p) = if (esAceitunas i)
                                 then Capa (dobleAceitunas i) (duplicarAceitunas p)
                                 else Capa i (duplicarAceitunas p)

esAceitunas :: Ingrediente -> Bool
esAceitunas (Aceitunas _) = True
esAceitunas _             = False                                 

dobleAceitunas :: Ingrediente -> Ingrediente
--PRECONDICIONES= El ingrediente dado son aceitunas.
dobleAceitunas (Aceitunas n) = (Aceitunas (n*2))

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (x:xs) = (cantCapasDePizza x) : (cantCapasPorPizza xs)

cantCapasDePizza :: Pizza -> (Int, Pizza)
cantCapasDePizza p = ((cantidadDeCapas p), p)

--2. Mapa de tesoros (con bifurcaciones)

--una lista de Dir puede indicar las direcciones en las que hay que moverse por un mapa para llegar a cierto 
--punto de este
data Dir = Izq | Der 
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cof [Objeto]
    deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

--               (Bifurcacion [Chatarra, Chatarra] 
--  (Fin (Cof [Chatarra, Tesoro])) (Fin (Cof [Chatarra, Chatarra])))

mapita :: Mapa
mapita=(Bifurcacion (Cof [Chatarra, Chatarra]) (Fin (Cof [Chatarra, Chatarra])) (Fin (Cof [Chatarra, Chatarra])))

mapazo :: Mapa
mapazo= (Bifurcacion (Cof [Chatarra, Chatarra]) (Fin (Cof [Chatarra, Tesoro])) (Fin (Cof [Chatarra, Chatarra])))

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = tieneTesoro c
hayTesoro (Bifurcacion c m1 m2) = tieneTesoro c || hayTesoro m1 || hayTesoro m2

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cof xs) = tieneTesoroL xs

tieneTesoroL :: [Objeto] -> Bool
--PRECONDICIONES= La lista no es vacía.
tieneTesoroL (x:[]) = esTesoro x
tieneTesoroL (x:xs) = esTesoro x || tieneTesoroL xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

hayTesoroEn :: [Dir] -> Mapa -> Bool
--PRECONDICIONES = Existe un camino en el mapa a través las direcciones de la lista dada.
hayTesoroEn []     (Fin c)               = tieneTesoro c
hayTesoroEn []     (Bifurcacion c m1 m2) = tieneTesoro c
hayTesoroEn (x:xs) (Fin c)               = error ("No deberia terminarse aca. Aun hay direcciones restantes")
hayTesoroEn (x:xs) (Bifurcacion c m1 m2) = if (esIzq x) 
                                             then (hayTesoroEn xs m1)
                                             else (hayTesoroEn xs m2)

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False      

caminoAlTesoro :: Mapa -> [Dir]
--PRECONDICIONES= existe un tesoro y es único.
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if (tieneTesoro c)
                                         then []
                                         else (direccionAlCorrecto m1 m2) : (caminoAlTesoro (seguirElCorrecto m1 m2))

direccionAlCorrecto :: Mapa -> Mapa -> Dir
direccionAlCorrecto m1 m2= if (hayTesoro m1)
                             then Izq
                             else Der

seguirElCorrecto :: Mapa -> Mapa -> Mapa
--PRECONDICIONES= Uno de los dos mapas tiene tesoro
seguirElCorrecto m1 m2= if (hayTesoro m1)
                          then m1
                          else m2       

mapazo2 :: Mapa
mapazo2 = (Bifurcacion (Cof [Chatarra, Chatarra])
            (Bifurcacion (Cof [Chatarra, Chatarra]) (Fin (Cof [Chatarra, Tesoro])) (Fin (Cof [Tesoro, Chatarra])))
            (Fin (Cof [Chatarra, Tesoro])) )     

mapazo3 :: Mapa
mapazo3 = (Bifurcacion (Cof [Tesoro, Tesoro, Chatarra, Tesoro]) mapazo2 (Fin (Cof [Chatarra, Tesoro])))                             

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c)               = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if (esMasLarga (caminoDeLaRamaMasLarga m1) (caminoDeLaRamaMasLarga m2))
                                               then (Izq : (caminoDeLaRamaMasLarga m1))
                                               else (Der : (caminoDeLaRamaMasLarga m2))

esMasLarga :: [a] -> [a] -> Bool
esMasLarga xs ys = (length xs) > (length ys)

--Sergio me corrigió en el tp 3 que debería hacer una recursión agregando nivel a nivel y no usando tesorosEnNivelN,
--recorriendo así múltiples veces el árbol. La idea es recorrer una vez sola.
--fijarse después de rehacerlo este en tp 3 (listPerLevel)

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)               = [tesorosAca (objetosDeCofre c)]
tesorosPorNivel (Bifurcacion c m1 m2) = (tesorosAca (objetosDeCofre c)) : (juntarNivelesDeTesoros (tesorosPorNivel m1) (tesorosPorNivel m2))

objetosDeCofre :: Cofre -> [Objeto]
objetosDeCofre (Cof xs) = xs

tesorosAca :: [Objeto] -> [Objeto]
tesorosAca []     = []
tesorosAca (x:xs) = if (esTesoro x)
                      then (Tesoro : (tesorosAca xs))
                      else (tesorosAca xs)

juntarNivelesDeTesoros :: [[Objeto]] -> [[Objeto]]  -> [[Objeto]] 
juntarNivelesDeTesoros xss []            = xss
juntarNivelesDeTesoros [] yss            = yss
juntarNivelesDeTesoros (xs:xss) (ys:yss) = (xs ++ ys) : (juntarNivelesDeTesoros xss yss)    

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _)               = []
todosLosCaminos (Bifurcacion _ m1 m2) = ([Izq] : (consACada Izq (todosLosCaminos m1)) ) ++
                                        ([Der] : (consACada Der (todosLosCaminos m2)) )

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss                                        

--3. Nave Espacial

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
data Nave = N (Tree Sector)
    deriving Show

nave007 :: Nave
nave007 = N (NodeT (S "a920" [LanzaTorpedos, (Almacen [Torpedo, Oxigeno])] ["michael", "enrique", "enriqueta"]) 
                (NodeT (S "34b0" [(Almacen [Torpedo, Comida, Comida]), (Motor 120)] ["enriqueta", "lewis"]) EmptyT EmptyT)
                (NodeT (S "890ab" [(Almacen [Combustible, Comida]), (Motor 450)] ["emilia", "enriqueta", "xing"]) EmptyT EmptyT))

sectores :: Nave -> [SectorId]
sectores (N ts) = sectoresDelTree ts

sectoresDelTree :: Tree Sector -> [SectorId]
sectoresDelTree EmptyT            = []
sectoresDelTree (NodeT s ts1 ts2) = sectoresIdDe s : ((sectoresDelTree ts1) ++ (sectoresDelTree ts2))

sectoresIdDe :: Sector -> SectorId
sectoresIdDe (S id _ _) = id

poderDePropulsion :: Nave -> Int
poderDePropulsion (N ts) = propulsionDelTree ts

propulsionDelTree :: Tree Sector -> Int
propulsionDelTree EmptyT            = 0
propulsionDelTree (NodeT s ts1 ts2) = propulsionSiHay s + (propulsionDelTree ts1) + (propulsionDelTree ts2) 

propulsionSiHay :: Sector -> Int
propulsionSiHay (S _ cs _) = propulsionDe cs

propulsionDe :: [Componente] -> Int 
propulsionDe []     = 0
propulsionDe (x:xs) = propulsionSiEsMotor x + (propulsionDe xs)

propulsionSiEsMotor :: Componente -> Int
propulsionSiEsMotor (Motor n) = n
propulsionSiEsMotor _         = 0

barriles :: Nave -> [Barril]
barriles (N ts1) = barrilesDelTree ts1

barrilesDelTree :: Tree Sector -> [Barril]
barrilesDelTree EmptyT            = []
barrilesDelTree (NodeT s ts1 ts2) = barrilesSiHay s ++ ((barrilesDelTree ts1) ++ (barrilesDelTree ts2))

barrilesSiHay :: Sector -> [Barril]
barrilesSiHay (S _ cs _) = barrilesDe cs

barrilesDe :: [Componente] -> [Barril]
barrilesDe []     = []
barrilesDe (x:xs) = if (esAlmacen x)
                      then (barrilesDelAlmacen x) ++ (barrilesDe xs) 
                      else (barrilesDe xs) 

esAlmacen :: Componente -> Bool
esAlmacen (Almacen _) = True
esAlmacen _           = False

barrilesDelAlmacen :: Componente -> [Barril]
--PRECONDICIÓN= El componente debe ser Almacen.
barrilesDelAlmacen (Almacen bs) = bs

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N ts) = N (sectoresConAgregadoSi cs id ts)

sectoresConAgregadoSi :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
sectoresConAgregadoSi cs id EmptyT            = EmptyT
sectoresConAgregadoSi cs id (NodeT s ts1 ts2) = if (sonLoMismo id (idDe s))
                                                  then (NodeT (sectorConAgregado cs s) (sectoresConAgregadoSi cs id ts1) (sectoresConAgregadoSi cs id ts2))
                                                  else (NodeT s (sectoresConAgregadoSi cs id ts1) (sectoresConAgregadoSi cs id ts2))

idDe :: Sector -> SectorId
idDe (S id _ _) = id

sonLoMismo :: Eq a => a -> a -> Bool
sonLoMismo x y = x==y

sectorConAgregado :: [Componente] -> Sector -> Sector
sectorConAgregado cs1 (S id cs2 ts) = (S id (cs2++cs1) ts)

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--PRECONDICIONES: Todos los id de la lista existen en la nave.
asignarTripulanteA t ids (N tree) = N (sectoresConTripulante t ids tree) 

sectoresConTripulante :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
sectoresConTripulante t ids EmptyT            = EmptyT
sectoresConTripulante t ids (NodeT s tree1 tree2) = if (esUnoDeLosSectores ids (idDe s))
                                                  then (NodeT (sectorConTripulante t s) (sectoresConTripulante t ids tree1) (sectoresConTripulante t ids tree2))
                                                  else (NodeT s (sectoresConTripulante t ids tree1) (sectoresConTripulante t ids tree2))

esUnoDeLosSectores :: [SectorId] -> SectorId -> Bool
esUnoDeLosSectores []     y = False
esUnoDeLosSectores (x:xs) y = sonLoMismo x y || esUnoDeLosSectores xs y 

sectorConTripulante :: Tripulante -> Sector -> Sector 
sectorConTripulante t (S id cs ts) = S id cs (t:ts) 

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N tree) = sectoresDe t tree

sectoresDe :: Tripulante -> Tree Sector -> [SectorId]
sectoresDe t EmptyT                          = []
sectoresDe t (NodeT (S id _ ts) tree1 tree2) = if (pertenece t ts)
                                                then (id : (sectoresDe t tree1) ++ (sectoresDe t tree2))
                                                else (sectoresDe t tree1) ++ (sectoresDe t tree2)

pertenece :: Eq a => a -> [a] -> Bool
pertenece _  []    = False
pertenece x (y:ys) = (x==y) || pertenece x ys 

tripulantes :: Nave -> [Tripulante]
tripulantes (N tree) = tripulantesSinRepes (tripulantesDeSectores tree)

tripulantesDeSectores :: Tree Sector -> [Tripulante]
tripulantesDeSectores EmptyT                = []
tripulantesDeSectores (NodeT s tree1 tree2) = tripulantesDeSector s ++ (tripulantesDeSectores tree1) ++ (tripulantesDeSectores tree2)

tripulantesDeSector :: Sector -> [Tripulante]
tripulantesDeSector (S _ _ ts) = ts

tripulantesSinRepes :: [Tripulante] -> [Tripulante]
tripulantesSinRepes []     = []
tripulantesSinRepes (x:xs) = if (noSeRepiteEn x xs)
                               then x : (tripulantesSinRepes xs)
                               else tripulantesSinRepes xs

noSeRepiteEn :: Eq a => a -> [a] -> Bool
noSeRepiteEn x []     = True
noSeRepiteEn x (y:ys) = x/=y && noSeRepiteEn x ys                                

--4. Manada de lobos

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
    deriving Show
data Manada = M Lobo
    deriving Show

loberioFeroz :: Manada
loberioFeroz = M (Cazador "Colmillonatan" ["Despistadeo", "Perezosandra", "Papanatalia"]
                    (Cria "Chiquilin") (Explorador "Astutobias" ["Parque Yellowstone", "Bosque Verdoso"] 
                                            (Cria "Enanin") (Explorador "Astutomas" ["Bosque Verdoso", "Rio Azulado"]
                                                            (Cria "Petisin") (Cria "Chiquirrin"))) 
                                       (Cria "Chiquitin"))

lobosLocos :: Lobo
lobosLocos = (Cazador "Colmillonatan" ["Despistadeo", "Perezosandra", "Papanatalia"]
                    (Cria "Chiquilin") (Explorador "Astutobias" ["Parque Yellowstone", "Rio Mojado"] 
                                            (Cria "Enanin") (Explorador "Astutomas" ["Bosque Verdoso", "Rio Azulado"]
                                                            (Cria "Petisin") (Cria "Chiquirrin"))) 
                                       (Cria "Chiquitin"))                                       

buenaCaza :: Manada -> Bool
buenaCaza (M l) = (nroPresasDe l) > (nroCriasDe l)

nroPresasDe :: Lobo -> Int
nroPresasDe (Cria _)                = 0
nroPresasDe (Explorador _ _ l1 l2)  = (nroPresasDe l1) + (nroPresasDe l2)
nroPresasDe (Cazador _ ps l1 l2 l3) = (presasAca ps) + (nroPresasDe l1) + (nroPresasDe l2) +(nroPresasDe l3)

presasAca :: [Presa] -> Int
presasAca ps = length ps

nroCriasDe :: Lobo -> Int
nroCriasDe (Cria _)               = 1
nroCriasDe (Explorador _ _ l1 l2) = (nroCriasDe l1) + (nroCriasDe l2)
nroCriasDe (Cazador _ _ l1 l2 l3) = (nroCriasDe l1) + (nroCriasDe l2) + (nroCriasDe l3)

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaDe l

elAlfaDe :: Lobo -> (Nombre, Int)
elAlfaDe (Cria n)                = (n,0)
elAlfaDe (Explorador n _ l1 l2)  = elMejorEntre (n,0) (elMejorEntre (elAlfaDe l1) (elAlfaDe l2))
elAlfaDe (Cazador n ps l1 l2 l3) = elMejorEntre (n, (presasAca ps)) (elMejorEntre (elAlfaDe l1) (elMejorEntre (elAlfaDe l2) (elAlfaDe l3)))

elMejorEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elMejorEntre x y = if (snd(x) >= snd(y))
                     then x
                     else y

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = (losQueExploraronDe t l)

losQueExploraronDe :: Territorio -> Lobo -> [Nombre]
losQueExploraronDe t (Cria _)                = []
losQueExploraronDe t (Explorador n ts l1 l2) = (singularSi n (pertenece t ts) ) ++ (losQueExploraronDe t l1) ++ (losQueExploraronDe t l2)
losQueExploraronDe t (Cazador _ _ l1 l2 l3)  = (losQueExploraronDe t l1) ++ (losQueExploraronDe t l2) ++ (losQueExploraronDe t l3)

singularSi :: a -> Bool -> [a]
singularSi x True  = [x]
singularSi _ False = []

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = (exploradoresPorCadaTerritorio (nombresYTerritorios l) [])

nombresYTerritorios :: Lobo -> [(Nombre, [Territorio])]
nombresYTerritorios (Cria _)                = []
nombresYTerritorios (Explorador n ts l1 l2) = (n, ts) : (nombresYTerritorios l1) ++ (nombresYTerritorios l2)
nombresYTerritorios (Cazador _ _ l1 l2 l3)  = (nombresYTerritorios l1) ++ (nombresYTerritorios l2) ++ (nombresYTerritorios l3)

exploradoresPorCadaTerritorio :: [(Nombre, [Territorio])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
exploradoresPorCadaTerritorio [] ys     = ys
exploradoresPorCadaTerritorio (x:xs) ys = exploradoresPorCadaTerritorio xs (expComputado (fst(x)) (snd(x)) ys) 

expComputado :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
expComputado _ []     xs = xs
expComputado n (t:ts) xs = expComputado n ts (terComputado n t xs)

terComputado :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
terComputado n t []     = [(t, [n])]
terComputado n t (x:xs) = if (t==fst(x)) 
                            then ((fst(x),n:(snd(x))):xs)
                            else x : (terComputado n t xs)

loberioFeroz' :: Manada
loberioFeroz' = M (Cazador "Colmillonazo" ["Despistadeo", "Perezosandra", "Papanatalia"]
                    (Cria "Chiquilin") (Explorador "Astutobias" ["Parque Yellowstone", "Rio Mojado"] 
                                            (Cria "Enanin") (Explorador "Astutomas" ["Bosque Verdoso", "Rio Azulado"]
                                                            (Cria "Petisin") (Cazador "Colmillonatan" ["Despistadeo", "Perezosandra", "Papanatalia"]
                                                                             (Cria "Chiquilin") (Cria "Chiquilin") (Cria "Chiquilin")))) 
                                       (Cria "Chiquitin"))  

lobosLocos' :: Lobo
lobosLocos' = (Cazador "Colmillonazo" ["Despistadeo", "Perezosandra", "Papanatalia"]
                    (Cria "Chiquilin") (Explorador "Astutobias" ["Parque Yellowstone", "Rio Mojado"] 
                                            (Cria "Enanin") (Explorador "Astutomas" ["Bosque Verdoso", "Rio Azulado"]
                                                            (Cria "Petisin") (Cazador "Colmillonatan" ["Despistadeo", "Perezosandra", "Papanatalia"]
                                                                             (Cria "Chiquilin") (Cria "Chiquilin") (Cria "Chiquilin")))) 
                                       (Cria "Chiquitin"))                                  

superioresDelCazador :: Nombre -> Manada -> [Nombre]
--Precondición: hay un cazador con dicho nombre y es único.   
superioresDelCazador n (M l) = superioresAlNivel (nivelEnTree n 0 l) l

nivelEnTree :: Nombre -> Int -> Lobo -> Int
--Precondición: hay un cazador con dicho nombre y es único.
nivelEnTree _ _ (Cria _)                   = 0
nivelEnTree nom num (Explorador _ _ l1 l2) = (nivelEnTree nom (num+1) l1) + (nivelEnTree nom (num+1) l2)
nivelEnTree nom num (Cazador n _ l1 l2 l3) = if (nom==n)
                                               then num
                                               else (nivelEnTree nom (num+1) l1) + (nivelEnTree nom (num+1) l2) + (nivelEnTree nom (num+1) l3)

superioresAlNivel :: Int -> Lobo -> [Nombre]
superioresAlNivel 0 _                        = []
superioresAlNivel _ (Cria _)                 = []
superioresAlNivel n (Explorador _ _ l1 l2)   = (superioresAlNivel (n-1) l1) ++ (superioresAlNivel (n-1) l2)
superioresAlNivel n (Cazador nom _ l1 l2 l3) = nom : (superioresAlNivel (n-1) l1) ++ (superioresAlNivel (n-1) l2) ++ (superioresAlNivel (n-1) l3)


