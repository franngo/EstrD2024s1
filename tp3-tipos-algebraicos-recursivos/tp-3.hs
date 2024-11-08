--1. Tipos recursivos simples

--1.1. Celdas con bolitas

data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia         = 0
nroBolitas col1 (Bolita col2 cel) = unoSi (esMismoColor col1 col2) + nroBolitas col1 cel

unoSi :: Bool -> Int
unoSi True  = 1 
unoSi False = 0

esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _    _    = False

poner :: Color -> Celda -> Celda
poner col cel = Bolita col cel

--(Bolita Rojo (Bolita Azul (Bolita Azul CeldaVacia)))

sacar :: Color -> Celda -> Celda
--OBSERVACIÓN= En caso de no haber bolitas del color dado en la celda dada, se describe la misma sin cambios.
sacar _     CeldaVacia       = CeldaVacia
sacar col1 (Bolita col2 cel) = if (esMismoColor col1 col2)
                                 then cel
                                 else (Bolita col2 (sacar col1 cel))

ponerN :: Int -> Color -> Celda -> Celda
ponerN num col cel = if (num>0)
                      then Bolita col (ponerN (num-1) col cel)
                      else cel

--1.2. Camino hacia el tesoro   

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

--(Nada (Cofre [Cacharro, Cacharro] (Nada (Nada (Cofre [Tesoro] (Nada Fin))))))
--(Nada (Cofre [Tesoro, Tesoro, Cacharro] (Nada (Nada (Cofre [Tesoro] (Nada Fin))))))

hayTesoro :: Camino -> Bool
hayTesoro Fin              = False
hayTesoro (Nada cam)       = hayTesoro cam
hayTesoro (Cofre objs cam) = contieneTesoro objs || hayTesoro cam

contieneTesoro :: [Objeto] -> Bool
contieneTesoro []     = False
contieneTesoro (x:xs) = esTesoro x || contieneTesoro xs 

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

pasosHastaTesoro :: Camino -> Int
--PRECONDICIÓN: Tiene que haber al menos un tesoro.
pasosHastaTesoro (Nada cam)       = 1 + pasosHastaTesoro cam
pasosHastaTesoro (Cofre objs cam) = if (contieneTesoro objs)
                                      then 0
                                      else 1 + pasosHastaTesoro cam

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0   (Cofre objs cam) = contieneTesoro objs
hayTesoroEn 0   _                = False
hayTesoroEn num Fin              = False
hayTesoroEn num (Nada cam)       = hayTesoroEn (num-1) cam
hayTesoroEn num (Cofre objs cam) = (hayTesoroEn (num-1) cam)

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros num cam = (cantidadDeTesorosEn cam) >= num

cantidadDeTesorosEn :: Camino -> Int
cantidadDeTesorosEn Fin              = 0
cantidadDeTesorosEn (Nada cam)       = cantidadDeTesorosEn cam
cantidadDeTesorosEn (Cofre objs cam) = (tesorosAca objs) + (cantidadDeTesorosEn cam)

tesorosAca :: [Objeto] -> Int
tesorosAca []     = 0
tesorosAca (x:xs) = (unoSi (esTesoro x)) + tesorosAca xs

--DESAFÍO

cantTesorosEntre :: Int -> Int -> Camino -> Int
--PRECONDICIÓN= n debe ser mayor o igual 0 y m mayor o igual a n.
cantTesorosEntre _  _  Fin              = 0
cantTesorosEntre 0  n2 (Nada cam)       = revisarHastaEl (n2 - 1) cam
cantTesorosEntre 0  n2 (Cofre objs cam) = (tesorosAca objs) + revisarHastaEl (n2 - 1) cam
cantTesorosEntre n1 n2 (Nada cam)       = cantTesorosEntre (n1 - 1) (n2 - 1) cam 
cantTesorosEntre n1 n2 (Cofre objs cam) = cantTesorosEntre (n1 - 1) (n2 - 1) cam

revisarHastaEl :: Int -> Camino -> Int 
revisarHastaEl _ Fin              = 0
revisarHastaEl 0 (Cofre objs cam) = (tesorosAca objs)
revisarHastaEl 0 _                = 0
revisarHastaEl n (Nada cam)       = revisarHastaEl (n - 1) cam
revisarHastaEl n (Cofre objs cam) = (tesorosAca objs) + revisarHastaEl (n - 1) cam

--2. Tipos arbóreos

--2.1. Árboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

--(NodeT 'c' (NodeT 'a' EmptyT EmptyT) (NodeT 'e' (NodeT 'u' EmptyT EmptyT) EmptyT))
--(NodeT 3 (NodeT 4 EmptyT EmptyT) (NodeT 3 (NodeT 9 EmptyT EmptyT) EmptyT))
--(NodeT 3 (NodeT 4 (NodeT 7 EmptyT EmptyT) EmptyT) (NodeT 3 (NodeT 9 EmptyT EmptyT) EmptyT))

sumarT :: Tree Int -> Int
sumarT EmptyT                = 0
sumarT (NodeT num nums1 nums2) = num + (sumarT nums1) + (sumarT nums2) 

sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT x ys zs) = 1 + (sizeT ys) + (sizeT zs)

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT                  = EmptyT
mapDobleT (NodeT num nums1 nums2) = (NodeT (num*2) (mapDobleT nums1) (mapDobleT nums2))

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT          = False
perteneceT p (NodeT x ys zs) = (p==x) || perteneceT p ys || perteneceT p zs

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT          = 0
aparicionesT p (NodeT x ys zs) = if (p==x)
                                 then 1 + (aparicionesT p ys) + (aparicionesT p zs)
                                 else (aparicionesT p ys) + (aparicionesT p zs)

leaves :: Tree a -> [a]
leaves EmptyT          = []
leaves (NodeT x ys zs) = if (esVacio ys && esVacio zs)
                           then [x]
                           else (leaves ys) ++ (leaves zs)

esVacio :: Tree a -> Bool
esVacio EmptyT = True
esVacio _      = False                           

heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + mayorEntre (heightT t1) (heightT t2)

mayorEntre :: Int -> Int -> Int
mayorEntre n1 n2 = if (n1 > n2)
                        then n1
                        else n2                          
                        
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x ys zs) = (NodeT x (mirrorT zs) (mirrorT ys))

toList :: Tree a -> [a]
toList EmptyT          = [] 
toList (NodeT x ys zs) = toList ys ++ [x] ++ toList zs

levelN :: Int -> Tree a -> [a]
--OBSERVACIÓN= Si árbol dado no tiene al menos n niveles -siendo n el parámetro de tipo Int dado-, se describe
--una lista vacía.
levelN _ EmptyT          = []
levelN 0 (NodeT x ys zs) = [x]
levelN n (NodeT x ys zs) = (levelN (n-1) ys) ++ (levelN (n-1) zs)

--(NodeT 3 (NodeT 4 (NodeT 7 EmptyT EmptyT) (NodeT 5 (NodeT 9 EmptyT EmptyT) EmptyT)) (NodeT 3 EmptyT EmptyT))
--(NodeT 3 (NodeT 4 (NodeT 7 EmptyT EmptyT) (NodeT 5 EmptyT EmptyT)) (NodeT 3 EmptyT EmptyT))

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : (juntarNivelesDe (listPerLevel t1) (listPerLevel t2))

juntarNivelesDe :: [[a]] -> [[a]] -> [[a]]
juntarNivelesDe  xss      []       = xss
juntarNivelesDe  []        yss     = yss
juntarNivelesDe (xs:xss) (ys:yss) = (xs++ys) : (juntarNivelesDe xss yss)

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x ys zs) = x : (laMasLargaEntre (ramaMasLarga ys) (ramaMasLarga zs))

laMasLargaEntre :: [a] -> [a] -> [a] 
laMasLargaEntre xs ys= if ((length xs) > (length ys))
                            then xs
                            else ys

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = [x] : (consACada x ((todosLosCaminos t1) ++ (todosLosCaminos t2)))

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss   

objetoDe :: Tree a -> [a]
objetoDe EmptyT        = []
objetoDe (NodeT x _ _) = [x]

primerTreeDe :: Tree a -> Tree a
primerTreeDe (NodeT _ t1 _) = t1

segundoTreeDe :: Tree a -> Tree a
segundoTreeDe (NodeT _ _ t2) = t2

--2.2. Expresiones aritméticas

data ExpA = Valor Int
          | Sum ExpA ExpA
          | Prod ExpA ExpA
          | Neg ExpA
    deriving Show

--(Sum (Valor 2) (Valor 3))
--(Prod (Valor 2) (Valor 3))

eval :: ExpA -> Int
eval (Valor x)      = x
eval (Sum op1 op2)  = (eval op1) + (eval op2)
eval (Prod op1 op2) = (eval op1) * (eval op2)
eval (Neg op1)      = (-(eval op1))

simplificar :: ExpA -> ExpA
simplificar (Valor x)      = Valor x
simplificar (Sum op1 op2)  = sumaSimplificada (simplificar op1) (simplificar op2)                                                  
simplificar (Prod op1 op2) = productoSimplificado (simplificar op1) (simplificar op2)                                                      
simplificar (Neg op1)      = negacionSimplificada (simplificar op1)
                                
sumaSimplificada :: ExpA -> ExpA -> ExpA
sumaSimplificada op1 (Valor 0) = op1
sumaSimplificada (Valor 0) op2 = op2
sumaSimplificada op1 op2       = Sum op1 op2

productoSimplificado :: ExpA -> ExpA -> ExpA
productoSimplificado op1 (Valor 0) = Valor 0
productoSimplificado (Valor 0) op2 = Valor 0
productoSimplificado op1 (Valor 1) = op1
productoSimplificado (Valor 1) op2 = op2
productoSimplificado op1 op2       = Prod op1 op2

negacionSimplificada :: ExpA -> ExpA
negacionSimplificada (Neg op) = op
negacionSimplificada op       = Neg op 