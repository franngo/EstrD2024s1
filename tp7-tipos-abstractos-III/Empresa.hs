module Empresa
    (Empresa, consEmpresa, buscarPorCUIL)
where

--Empleado es un TAD del que somos usuarios, siendo que un dato de tipo Empleado cuenta con un dato de tipo CUIL y una lista de SectorId
--que representa los sectores en los que trabaja el mismo.

{- INTERFAZ DE Empleado
consEmpleado :: CUIL -> Empleado
Propósito : construye un empleado con dicho CUIL.
Costo : O(1)

cuil :: Empleado -> CUIL
Propósito : indica el CUIL de un empleado.
Costo : O(1)

incorporarSector :: SectorId -> Empleado -> Empleado
Propósito : incorpora un sector al conjunto de sectores en los que trabaja un empleado.
Costo : O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.

sectores :: Empleado -> [SectorId]
Propósito : indica los sectores en los que el empleado trabaja.
Costo : O(S)
-}

--Este tipo también es usuario de Set y Map

--(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
              O(log n) O(log n) O(1)  O(log n)         O(n), xq debería pasar por todos los elementos del tree y agregarlos a la lista.
--(Map, emptyM, assocM, lookupM, deleteM, keys)
               O(log n) O(log n) O(log n) O(n) 

--tanto Map como Set exponen una interfaz eficiente con costos logarítmicos para inserción,
--búsqueda y borrado, tal cual vimos en clase.

--En los costos, S es la cantidad de sectores de la empresa, y E es la cantidad de empleados.

data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)
{-INV. REP.:
    *sea ConsE msise mce
    *No puede haber empleados que no estén asignados a ningún sector en mce.
    *Si un empleado aparece como valor de un set en al menos una de las asociaciones de msise, este mismo debe aparecer en una asociación 
     en mce.
    *Si un empleado aparece como valor en una asociación de mce, este mismo debe aparecer como valor de un set de al menos una asociación 
     de msise.
    *En cada asociación de msise, el sector id debe coincidir con alguno de los que poseen como valor para los sectores asignados todos los
     empleados del set relacionado.
    *En cada asociación de mce, el cuil debe coincidir con el que posee como valor para su cuil el empleado relacionado.
    *Si un sector id aparece como valor para los sectores asignados de un empleado de alguna asociación en mce, este mismo debe aparecer
    como valor en una asociación en msise.
-}

*Si un sector aparece como valor de un tripulante en mnt, este tiene que aparecer como valor 
    en mss.

--EJERCICIO 4 (IMPLEMENTADOR) =

consEmpresa :: Empresa                                              --Costo : O(1), porque no se realiza operación más que describir un
                                                                    --dato invariable.
consEmpresa = ConsE emptyM emptyM

buscarPorCUIL :: CUIL -> Empresa -> Empleado                        --Costo O(log E), ya que se realiza la operación con costo logarítmico
                                                                    --lookupM sobre la lista de asociaciones cuit-empleado.
--Propósito : devuelve el empleado con dicho CUIL.
--Precondición : el CUIL es de un empleado de la empresa.
buscarPorCUIL x (ConsE msise mce) = fromJust (lookupM x mce)

empleadosDelSector :: SectorId -> Empresa -> [Empleado]             --Costo esperado : O(log S + E)
                                                                    --Costo obtenido : O(log S + e), siendo e la cantidad de empleados de
                                                                    --del sector coincidente con la id dada. Esto es así porque se realiza
                                                                    --una operación logarítmica sobre msise -lookupM- y una lineal sobre 
                                                                    --los empleados del sector coincidente con la id dada -setToList-.
--Propósito : indica los empleados que trabajan en un sector dado.
empleadosDelSector s (ConsE msise mce) = let se = lookupM s msise
                                         in setToList se     

todosLosCUIL :: Empresa -> [CUIL]                                   --Costo : O(E), ya que se realiza una operación lineal sobre las
                                                                    --asociaciones CUIL-empleado -keys-, que son equivalentes a la cantidad
                                                                    --de empleados de la empresa.
--Propósito : indica todos los CUIL de empleados de la empresa.                                                
todosLosCUIL (ConsE msise mce) = keys mce

todosLosSectores :: Empresa -> [SectorId]                           --Costo : O(S), ya que se realiza una operación lineal sobre las
                                                                    --asociaciones sector id-empleados -keys-, que son equivalentes a la
                                                                    --cantidad de sectores de la empresa.
--Propósito : indica todos los sectores de la empresa.
todosLosSectores (ConsE msise mce) = keys msise

agregarSector :: SectorId -> Empresa -> Empresa                     --Costo : O(log S), ya que se realiza una operación logarítmica sobre
                                                                    --msise -assocM-, cuya cantidad de elementos equivale al total de
                                                                    --sectores de la empresa.
--Propósito : agrega un sector a la empresa, inicialmente sin empleados.
agregarSector s (ConsE msise mce) = assocM s EmptyS msise

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa         --Costo : O((s*log e)+(s* 3 log S+log m)+log E), siendo s la cantidad
                                                                    --de sectores en la lista dada, m la cantidad de empleados de cada
                                                                    --uno de estos sectores en msise y e la cantidad de sectores que el
                                                                    --empleado dado tiene asignados. Se realizan s operaciones logarítmicas
                                                                    --sobre e -incorporarSectores-, se realizan s veces 3 operaciones 
                                                                    --logarítmicas sobre S y una operación logarítmica sobre m
                                                                    -- -agregarASectores-, y se realiza una operación logarítmica sobre
                                                                    --E -assocM-.
--Propósito : agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el CUIL dado.
agregarEmpleado ss c (ConsE msise mce) = let e = incorporarSectores ss (consEmpleado c)
                                         in ConsE (agregarASectores e ss msise) (assocM c e mce)

incorporarSectores :: [SectorId] -> Empleado -> Empleado            --Costo O(s*log e), siendo s la cantidad de sectores en la lista dada 
                                                                    --y e la cantidad de sectores que el empleado dado tiene asignados. Se
                                                                    --realizan s operaciones logarítmicas sobre e --incorporarSector-.
incorporarSectores []     e = e
incorporarSectores (s:ss) e = incorporarSectores ss (incorporarSector s e)

---------------

agregarASectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado) 
                                                                    --Costo O(s* 3 log S+log m), siendo s la cantidad de sector ids
                                                                    --de la lista dada y m la cantidad de empleados de cada uno de 
                                                                    --estos sectores en msise. Se realizan s veces 3 operaciones
                                                                    --logarítmicas sobre S -2 lookupM y assocM- y una operación
                                                                    --logarítmica sobre m -addS-
agregarASectores e []     msise = msise
agregarASectores e (s:ss) msise = let es = fromJust (lookupM s msise)
                                      if (lookupM s msise /= Nothing)
                                        then agregarASectores e ss (assocM s (addS e es) msise)
                                        else agregarASectores e ss msise

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa            --Costo O((s*3 log S+log m)+2 log E+log s + s), siendo s la cantidad de 
                                                                    --sectores que el empleado del cuit dado tiene asignados + 1 y m la 
                                                                    --cantidad de empleados relacionados a cada uno de esos sectores en msise. 
                                                                    --Se realizan s veces 3 operaciones logarítmicas sobre S y una operación
                                                                    -- logarítmica sobre m -actualizarSectoresEmp-, se realizan 2 operaciones
                                                                    --logarítmicas sobre -E -lookupM y assocM-, se realizan una operación 
                                                                    --logarítmica sobre s --incorporarSector- y se realiza una operación
                                                                    --lineal sobre s -sectores-.
--Propósito : agrega un sector al empleado con dicho CUIL.
agregarASector s c (ConsE msise mce) = let e = incorporarSector s (fromJust (lookupM c mce))
                                       in ConsE (actualizarSectoresEmp (s:(sectores e)) e msise) (assocM c e mce)

actualizarSectoresEmp :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
                                                                    --Costo O(s*3 log S+log m), siendo s la cantidad de sectores en la
                                                                    --lista dada y m la cantidad de empleados relacionados a cada uno de
                                                                    --los sectores de la lista en msise. Se realizan s veces 3 operaciones
                                                                    --logarítmicas sobre S -2 lookupM y assocM- y una operación
                                                                    --logarítmica sobre m -addS-
actualizarSectoresEmp _      e msise = msise
actualizarSectoresEmp (s:ss) e msise = let es = fromJust (lookupM s msise)
                                       in if (lookupM s msise /= Nothing)
                                            then actualizarSectoresEmp ss e (assocM s (addS e es) msise) 
                                            else actualizarSectoresEmp ss e msise     

borrarEmpleado :: CUIL -> Empresa -> Empresa                        --Costo O((s * 3 log S + log m) + 2 log E + s), siendo s la cantidad
                                                                    --de sectores asignados al empleado dado y m la cantidad de empleados
                                                                    --relacionados a cada uno de estos sectores en msise.
                                                                    --Se realizan s veces 3 operaciones logarítmicas sobre S y una
                                                                    --operación logarítmica sobre m -borrarDeSectores-, se realizan 2 
                                                                    --operaciones logarítmicas sobre E -lookupM y deleteM- y se realiza una
                                                                    --operación lineal sobre s -sectores-.
--Propósito : elimina al empleado que posee dicho CUIL.
borrarEmpleado c (ConsE msise mce) = let e = fromJust (lookupM c mce)
                                     in ConsE (borrarDeSectores e (sectores e) msise) (deleteM c mce)                                                     

borrarDeSectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
                                                                    --Costo O(s * 3 log S + log m), siendo s la cantidad de sectores
                                                                    --asignados del empleado dado y m la cantidad de empleados relacionados
                                                                    --a cada uno de estos sectores en msise. Se realizan s veces 3 
                                                                    --operaciones logarítmicas sobre S -2 lookupM y un assocM- y una
                                                                    --operación logarítmica sobre m -removeS-.
borrarDeSectores e _      msise = msise
borrarDeSectores e (s:ss) msise = let esSinE= removeS e (fromJust (lookupM s msise))
                                  in if (lookupM s msise /= Nothing)
                                       then borrarDeSectores c ss (assocM s esSinE msise)
                                       else borrarDeSectores c ss msise  

--EJERCICIO 5 (USUARIO) =   

comenzarCon :: [SectorId] -> [CUIL] -> Empresa
--Costo O(s' log S + c * ((s*log e)+(s* 3 log S+log m)+log E) ), siendo s' la cantidad de sectorIds de la lista dada y siendo c la
--cantidad de cuils de la lista dada.                                                              
--Propósito : construye una empresa con la información de empleados dada. Los sectores no tienen empleados.
comenzarCon ss cs = let emp = (añadirSecEmpNueva ss ConsEmpresa)
                    in añadirEmplEmpNueva cs emp

añadirSecEmpNueva :: [SectorId] -> Empresa -> Empresa 
--Costo O(s log S), siendo s la cantidad de sectorIds de la lista dada. Esto es así porque se realizan s operaciones logarítmicas sobre
--S -agregarSector-.  
añadirSecEmpNueva _      emp = emp
añadirSecEmpNueva (s:ss) emp = añadirSecEmpNueva ss (agregarSector s emp)

añadirEmplEmpNueva :: [CUIL] -> Empresa -> Empresa
--Costo O(c * ((s*log e)+(s* 3 log S+log m)+log E)), siendo c la cantidad de cuils de la lista dada.  
añadirEmplEmpNueva _      emp = emp
añadirEmplEmpNueva (c:cs) emp = añadirEmplEmpNueva cs (agregarEmpleado [] c emp)

recorteDePersonal :: Empresa -> Empresa
--Costo : O(e + e * ((s * 3 log S + log m) + 2 log E + s) + 3 E), siendo e la mitad de los empleados de la empresa dada. Esto es así porque 
--se realiza una operación lineal sobre e -recortar-, otra de costo ((s * 3 log S + log m) + 2 log E + s) sobre e  -borrarEmpleados-, y 
--otras 3 operaciones lineales sobre E -length y 2 veces todosLosCUIL-.
--Propósito : dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
recorteDePersonal emp = let cuilsRecor = recortar (todosLosCUIL emp) (div (length (todosLosCUIL emp)) 2)
                        in borrarEmpleados cuilsRecor emp

recortar :: [a] -> Int -> [a]
--Costo O(n), siendo n el int dado. Esto es así porque, en el peor caso, se realizan n operaciones constantes. 
recortar xs     0 = xs
recortar []     n = []
recortar (x:xs) n = recortar xs (n-1)      

borrarEmpleados :: [CUIL] -> Empresa -> Empresa 
--Costo O(e * ((s * 3 log S + log m) + 2 log E + s)), siendo e la mitad de los empleados de la empresa dada. Esto es así porque se realizan 
--e operaciones de costo ((s * 3 log S + log m) + 2 log E + s).
borrarEmpleados []     emp = emp
borrarEmpleados (c:cs) emp = borrarEmpleados cs (borrarEmpleado c emp)

convertirEnComodin :: CUIL -> Empresa -> Empresa
--Costo O(S + ((S*log e)+(S* 3 log S+log m)+log E) ), siendo m la cantidad de empleados de cada uno de los sectores de la empresa y 
--siendo e la cantidad de sectores que el empleado del cuil dado tiene asignados.
convertirEnComodin c emp = let ss = todosLosSectores emp
                           in agregarEmpleado ss c emp 

esComodin :: CUIL -> Empresa -> Bool
--Costo O(S+ log E + s), siendo s la cantidad de sectores que el empleado del cuil dado tiene asignados. Esto es así porque se realiza una 
--operación lineal sobre S -todosLosSectores-, una logarítmica sobre E -buscarPorCUIL- y una lineal sobre s -sectores-.
--Propósito : dado un CUIL de empleado indica si el empleado está en todos los sectores.                           
esComodin c emp = let ss = todosLosSectores emp
                  in ss == sectores (buscarPorCUIL c emp)

------------

INTERFAZ DE Empleado
consEmpleado :: CUIL -> Empleado
Propósito : construye un empleado con dicho CUIL.
Costo : O(1)

cuil :: Empleado -> CUIL
Propósito : indica el CUIL de un empleado.
Costo : O(1)

incorporarSector :: SectorId -> Empleado -> Empleado
Propósito : incorpora un sector al conjunto de sectores en los que trabaja un empleado.
Costo : O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.

sectores :: Empleado -> [SectorId]
Propósito : indica los sectores en los que el empleado trabaja.
Costo : O(S)

--DEFINICIONES AUXILIARES =

type SectorId = Int
type CUIL = Int

--Empleado es un TAD (no conocemos su representación intera exacta)