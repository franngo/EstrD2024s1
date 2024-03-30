sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns

conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (b:bs) = b && conjuncion bs

disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs

aplanar :: [[a]] -> [a]
aplanar []         = []
aplanar (xs:xss) = xs ++ aplanar xss

pertenece :: Eq a => a -> [a] -> Bool
pertenece _  []    = False
pertenece x (y:ys) = (x==y) || pertenece x ys                   

apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0
apariciones x (y:ys) = unoSi (x==y) + apariciones x ys

unoSi :: Bool -> Int
unoSi True  = 1 
unoSi False = 0

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []       = [] 
losMenoresA num (n:ns) = if (n<num)
                           then n : losMenoresA num ns
                           else losMenoresA num ns

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = []
lasDeLongitudMayorA x (y:ys) = if ((longitud y) > x)
                                 then y : lasDeLongitudMayorA x ys
                                 else lasDeLongitudMayorA x ys

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y     = [y]
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y

agregar :: [a] -> [a] -> [a]
agregar [] []         = []
agregar [] (y:ys)     = y : agregar [] ys
agregar (x:xs) (y:ys) = x : agregar xs (y:ys)

reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = last (x:xs) : reversa (sinElUltimo (x:xs))

sinElUltimo :: [a] -> [a]
--PRECONDICIONES= La lista empleada como argumento no debe ser vacia.
sinElUltimo (x:[])     = [] 
sinElUltimo (x:xs) = x : sinElUltimo xs

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _          = []
zipMaximos _ []          = []
zipMaximos (x:xs) (y:ys) = if (x>=y)
                             then x : zipMaximos xs ys
                             else y : zipMaximos xs ys

elMinimo :: Ord a => [a] -> a
--PRECONDICIONES= La lista empleada como argumento no debe ser vacia.
elMinimo (x:[])   = x
elMinimo (x:e:xs) = if (x<e)
                    then elMinimo (x:xs)
                    else elMinimo (e:xs)

--2. RECURSIÓN SOBRE NÚMEROS                    

factorial :: Int -> Int
--PRECONDICIONES= El número empleado como argumento no debe ser negativo.
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if (n>=1)
                    then n : cuentaRegresiva (n-1)
                    else []                   

repetir :: Int -> a -> [a]
--PRECONDICIONES= El número empleado como argumento no debe ser negativo.
repetir 0 _ = []
repetir n e = e : repetir (n-1) e  

losPrimeros :: Int -> [a] -> [a]
--PRECONDICIONES= El número empleado como argumento no debe ser negativo.
losPrimeros 0 _      = []
losPrimeros _ []     = []   
losPrimeros n (x:xs) = x : (losPrimeros (n-1) xs)

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs      = xs
sinLosPrimeros _ []      = []
sinLosPrimeros n (x:xs)  = sinLosPrimeros (n-1) xs

--3. REGISTROS

data Persona = ConsPersona String Int
                         --nombre edad
    deriving Show

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ []      = [] 
mayoresA n (x:xs)  = if ((edadDe x) > n)
                       then x : mayoresA n xs
                       else mayoresA n xs                     

edadDe :: Persona -> Int
edadDe (ConsPersona _ n) = n             

promedioEdad :: [Persona] -> Int
--PRECONDICIONES = La lista al menos posee una persona.
promedioEdad xs = div (sumaDeEdadesDe xs) (longitud xs)

sumaDeEdadesDe :: [Persona] -> Int
sumaDeEdadesDe []     = 0
sumaDeEdadesDe (x:xs) = edadDe x + sumaDeEdadesDe xs 

elMasViejo :: [Persona] -> Persona
--PRECONDICIONES = La lista al menos posee una persona.
elMasViejo (x:[])     = x
elMasViejo (x:e:xs)   = if (edadDe x > edadDe e)
                          then elMasViejo (x:xs)
                          else elMasViejo (e:xs)

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int
    deriving Show
data Entrenador = ConsEntrenador String [Pokemon]
    deriving Show

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ xs) = cantPokeEnLista xs

cantPokeEnLista :: [Pokemon] -> Int
cantPokeEnLista []     = 0
cantPokeEnLista (x:xs) = 1 + (cantPokeEnLista xs)

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe x (ConsEntrenador _ ys) = cantPokeEnListaDe x ys

cantPokeEnListaDe :: TipoDePokemon -> [Pokemon] -> Int
cantPokeEnListaDe _ []     = 0
cantPokeEnListaDe x (y:ys) = if (sonMismoTipo x (tipoDe y))
                               then 1 + cantPokeEnListaDe x ys
                               else cantPokeEnListaDe x ys

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon x _) = x                         

sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Fuego Fuego   = True
sonMismoTipo Agua Agua     = True
sonMismoTipo Planta Planta = True
sonMismoTipo _ _           = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ x (ConsEntrenador _ ys) (ConsEntrenador _ zs) = cuantosPokeDeTipo_De_LeGananATodosLosDe_ x ys zs

cuantosPokeDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cuantosPokeDeTipo_De_LeGananATodosLosDe_ _ [] _      = 0
cuantosPokeDeTipo_De_LeGananATodosLosDe_ x (y:ys) zs = if ((sonMismoTipo x (tipoDe y)) && (leGanaAtodos y zs))
                                                         then 1 + cuantosPokeDeTipo_De_LeGananATodosLosDe_ x ys zs
                                                         else cuantosPokeDeTipo_De_LeGananATodosLosDe_ x ys zs

leGanaAtodos :: Pokemon -> [Pokemon] -> Bool
leGanaAtodos x [] = True
leGanaAtodos x (y:ys)  = if (tipo_SuperaA_ (tipoDe x) (tipoDe y)) 
                           then leGanaAtodos x ys
                           else False

tipo_SuperaA_ :: TipoDePokemon -> TipoDePokemon -> Bool
tipo_SuperaA_ Agua Fuego = True
tipo_SuperaA_ Fuego Planta = True
tipo_SuperaA_ Planta Agua = True
tipo_SuperaA_ _ _ = False

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ xs) = hayDeTodosLosTipos xs

hayDeTodosLosTipos :: [Pokemon] -> Bool
hayDeTodosLosTipos xs = (hayDeTipo Planta xs) && (hayDeTipo Agua xs) && (hayDeTipo Fuego xs)

hayDeTipo :: TipoDePokemon -> [Pokemon] -> Bool
hayDeTipo x [] = False
hayDeTipo x (y:ys) = if (sonMismoTipo x (tipoDe y))
                       then True
                       else hayDeTipo x ys

data Seniority = Junior | SemiSenior | Senior
    deriving Show
data Proyecto = ConsProyecto String
    deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show
data Empresa = ConsEmpresa [Rol]
    deriving Show

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa xs)= proyectosDeLista (proyectosDeRoles xs)

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles []     = []
proyectosDeRoles (x:xs) = proyectoDeRol x : proyectosDeRoles xs

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ x) = x
proyectoDeRol (Management _ x) = x

proyectosDeLista :: [Proyecto] -> [Proyecto]
proyectosDeLista []     = []
proyectosDeLista (x:xs) = if (estaRepetido x xs)
                            then proyectosDeLista xs
                            else x : proyectosDeLista xs

estaRepetido :: Proyecto -> [Proyecto] -> Bool
estaRepetido _ []     = False
estaRepetido x (y:ys) = if (sonMismoProyecto x y)
                          then True
                          else estaRepetido x ys

sonMismoProyecto :: Proyecto -> Proyecto -> Bool
sonMismoProyecto (ConsProyecto x) (ConsProyecto y) = sonIguales x y                      

sonIguales :: Eq a => a -> a -> Bool
sonIguales x y = x==y                                         

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa xs) ys = losDevSeniorDeLista xs ys

losDevSeniorDeLista :: [Rol] -> [Proyecto] -> Int
losDevSeniorDeLista [] ys     = 0
losDevSeniorDeLista (x:xs) ys = if ((esSenior x) && (perteneceA x ys))
                                  then 1 + losDevSeniorDeLista xs ys
                                  else losDevSeniorDeLista xs ys

esSenior :: Rol -> Bool
esSenior (Developer Senior _) = True
esSenior (Management Senior _) = True
esSenior _            = False 

perteneceA :: Rol -> [Proyecto] -> Bool
perteneceA _ []     = False 
perteneceA x (y:ys) = if (estaEnElProyecto x y)
                        then True
                        else perteneceA x ys

estaEnElProyecto :: Rol -> Proyecto -> Bool 
estaEnElProyecto (Developer _ (ConsProyecto x)) (ConsProyecto y) = sonIguales x y
estaEnElProyecto (Management _ (ConsProyecto x)) (ConsProyecto y) = sonIguales x y

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn xs (ConsEmpresa ys) = cantQueTrabajanEnDeLista xs ys

cantQueTrabajanEnDeLista :: [Proyecto] -> [Rol] -> Int
cantQueTrabajanEnDeLista _ []      = 0
cantQueTrabajanEnDeLista xs (y:ys) = if (perteneceA y xs)
                                       then 1 + cantQueTrabajanEnDeLista xs ys
                                       else cantQueTrabajanEnDeLista xs ys

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa xs) = emparejar (proyectosDeLista (proyectosDeRoles xs)) (numDeIntegrantesDeEn xs (proyectosDeLista (proyectosDeRoles xs)) )

emparejar :: [Proyecto] -> [Int] -> [(Proyecto, Int)]
emparejar []     _      = []
emparejar _      []     = []
emparejar (x:xs) (y:ys) = (x,y) : emparejar xs ys

numDeIntegrantesDeEn :: [Rol] -> [Proyecto] -> [Int]
numDeIntegrantesDeEn _  []     = []
numDeIntegrantesDeEn xs (y:ys) = numDeIntegrantesDeEnEste xs y : numDeIntegrantesDeEn xs ys

numDeIntegrantesDeEnEste :: [Rol] -> Proyecto -> Int
numDeIntegrantesDeEnEste []     _ = 0
numDeIntegrantesDeEnEste (x:xs) y = if (estaEnElProyecto x y)
                                      then 1 + numDeIntegrantesDeEnEste xs y
                                      else numDeIntegrantesDeEnEste xs y