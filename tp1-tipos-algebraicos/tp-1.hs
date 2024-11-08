--EJERCICIO 1

sucesor :: Int -> Int
sucesor x = x + 1

sumar :: Int -> Int -> Int
sumar x y = x + y

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)

maxDelPar :: (Int, Int) -> Int
maxDelPar (x,y) = if (x > y)
                    then x
                    else y  

--EJERCICIO 2

--sumar (maxDelPar(divisionYResto 10 3)) (sucesor 6)
--sucesor (maxDelPar (divisionYResto 18 (sumar (-3)5)))
--maxDelPar(divisionYResto (sumar 17 13) (sucesor 19))
--sumar (maxDelPar (divisionYResto (sumar  (sucesor 0) 9) 2)) (sucesor(sucesor 3))

--EJERCICIO 3.1

data Dir = Norte | Este | Sur | Oeste
    deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este  = Oeste
opuesto Sur   = Norte
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este   = True
iguales Sur Sur     = True
iguales Oeste Oeste = True
iguales _ _         = False

siguiente :: Dir -> Dir
--PRECONDICIONES= El parametro no puede tomar Oeste como valor.
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Norte
{-Esta funcion es parcial, ya que contempla restricciones en cuanto a los valores validos para pasar como
argumentos.-}

--EJERCICIO 3.2

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues x y = (valorNumericoDeDia x)>(valorNumericoDeDia y)

valorNumericoDeDia :: DiaDeSemana -> Int
valorNumericoDeDia Lunes     = 0
valorNumericoDeDia Martes    = 1
valorNumericoDeDia Miercoles = 2
valorNumericoDeDia Jueves    = 3
valorNumericoDeDia Viernes   = 4
valorNumericoDeDia Sabado    = 5
valorNumericoDeDia Domingo   = 6

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio _       = True

--EJERCICIO 3.3

negar :: Bool -> Bool
negar False = True
negar True  = False

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien _ _ = True

--EJERCICIO 4.1

data Persona = P String Int
               --Nombre Edad
    deriving Show

nombre :: Persona -> String
nombre (P n _) = n 

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = (P n (e+1) )

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoN (P n e) = (P nuevoN e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = if (e1>e2)
                                        then True
                                        else False

laQueEsMayor :: Persona -> Persona -> Persona
--PRECONDICIONES= Ambos argumentos empleados deben tener un valor diferente para el campo de edad.
laQueEsMayor (P n1 e1) (P n2 e2) = if (e1>e2)
                                    then (P n1 e1)
                                    else (P n2 e2)

--EJERCICIO 4.2

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Pokemon =  Poke TipoDePokemon Int
                                   --Porcentaje de energia
    deriving Show

data Entrenador = Ent String Pokemon Pokemon
                      --nombre poke1 poke2
    deriving Show

superaA :: Pokemon -> Pokemon -> Bool
superaA (Poke tipo1 _) (Poke tipo2 _) = tipo_SuperaA_ tipo1 tipo2

tipo_SuperaA_ :: TipoDePokemon -> TipoDePokemon -> Bool
tipo_SuperaA_ Agua Fuego = True
tipo_SuperaA_ Fuego Planta = True
tipo_SuperaA_ Planta Agua = True
tipo_SuperaA_ _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (Ent _ poke1 poke2) = cantidadDeTipoEn tipo poke1 poke2

cantidadDeTipoEn :: TipoDePokemon -> Pokemon -> Pokemon -> Int
cantidadDeTipoEn tipoDado (Poke tipo1 _) (Poke tipo2 _) = (unoSiSonMismoTipo tipoDado tipo1) + (unoSiSonMismoTipo tipoDado tipo2)

unoSiSonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
unoSiSonMismoTipo Planta Planta = 1
unoSiSonMismoTipo Fuego Fuego   = 1
unoSiSonMismoTipo Agua Agua     = 1
unoSiSonMismoTipo _ _           = 0

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((Ent _ poke1 poke2), (Ent _ poke3 poke4)) = [poke1, poke2, poke3, poke4]

--EJERCICIO 5.1

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)
{-Se usan 2 variables de tipo diferentes debido a que las tuplas son estructuras de datos que contienen 2 
datos que no necesariamente deben ser del mismo tipo-}

{-EJERCICIO 5.2= Estas funciones son polimorficas porque no definen un tipo de dato especifico que deban
poseer los argumentos empleados al llamar a las mismas. De este modo, se pueden utilizar argumentos de
cualquier tipo al llamarlas y no se producira un error de tipo.-}

--EJERCICIO 6

estaVacia :: [a] -> Bool
estaVacia (_:_) = False
estaVacia _ = True

elPrimero :: [a] -> a
--PRECONDICIONES= La lista empleada como argumento no debe ser vacia.
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
--PRECONDICIONES= La lista empleada como argumento no debe ser vacia.
sinElPrimero (x:y) = y

splitHead :: [a] -> (a, [a])
--PRECONDICIONES= La lista empleada como argumento no debe ser vacia.
splitHead (x:y) = (x,y)