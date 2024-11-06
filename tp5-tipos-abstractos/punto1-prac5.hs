--1. Cálculo de costos

head' :: [a] -> a --constante
head' (x:xs) = x 

sumar :: Int -> Int                             --constante
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

factorial :: Int -> Int           --lineal
factorial 0 = 1
factorial n = n * factorial (n-1) --constante
--O(n) porque, dado un número n, se realizan n operaciones constantes.

longitud :: [a] -> Int            --lineal
longitud [] = 0
longitud (x:xs) = 1 + longitud xs --constante

factoriales :: [Int] -> [Int]                     --O(N*X), ya que si llamás N a la cantidad de elementos, y X al valor más grande de la 
                                                  --lista, tenés N operaciones de costo O(X)
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
--O(n*m+1+1) o simplemente O(n*m) siendo n la cantidad de elementos de la lista y m el elemento más grande de la lista (esto porque nos concentramos
--en el peor caso de las distintas cosas que tenemos). 
--el peor caso de la lista es que se recorran todos sus elementos, lo cual se representa en n, y el peor caso de los elementos de la lista es
--el elemento más grande de la misma (ya que implica más operaciones), lo cual se representa en m.

--multiplicamos cuando por cada elemento de una estructura se hacen x operaciones, y se suma 1 por cada operación constante adicional que se le 
haga, como pm o cons (esto último es opcional ponerlo en la operación. Podemos solo específicarlo/mencionarlo en la justificación).      

pertenece :: Eq a => a -> [a] -> Bool         --lineal
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs --constante

sinRepetidos :: Eq a => [a] -> [a]                --cuadrático
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs           --lineal
                        then sinRepetidos xs
                        else x : sinRepetidos xs

{- La otra versión donde capáz se ve más claro
sinRepetidos :: [a]-> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = agregarONo x (sinRepetidos xs)

agregarONo :: a -> [a] -> [a]
agregarONo x [] = [x]
agregarONo x (y:ys) = if (x==y)
                        then ys
                        else y : (agregarONo x ys)
-}                        

--sinRepetidos tiene COSTO CUADRÁTICO, osea, O(n^2) 
--Creo que el peor de los casos de la lista es que se recorran todos los elementos, representado en n, y el peor caso de los elementos es que ninguno de estos
--esté repetido en la lista inicial, por lo que el primer recorrido implicaría n operaciones constantes, de modo que, como nos centramos en el peor caso de los
--elementos, se asume que "cada recorrido tiene costo n", por lo que el costo total de la operación sería de n*n (cuadrática).

-- equivalente a (++)  
append :: [a] -> [a] -> [a]         --lineal
append [] ys = ys
append (x:xs) ys = x : append xs ys --constante

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
--COSTO O(N*M), siendo n la cantidad de elementos de la lista dada y m la cantidad de elementos de la cadena empleada en la operación lineal append.

takeN :: Int -> [a] -> [a]          --lineal/O(n), siendo n la cantidad de elementos de la lista dada.
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs --constante

dropN :: Int -> [a] -> [a]      --lineal/O(n), siendo n la cantidad de elementos de la lista dada.
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs --constante

partir :: Int -> [a] -> ([a], [a])     --O(n*2), siendo n la cantidad de elementos presentes en la lista dada.
partir n xs = (takeN n xs, dropN n xs) --takeN y dropN son ambas lineales

minimo :: Ord a => [a] -> a       --lineal/O(n), siendo n la cantidad de elementos de la lista dada.
minimo [x] = x
minimo (x:xs) = min x (minimo xs) --constante

sacar :: Eq a => a -> [a] -> [a]        --lineal/O(n), siendo n la cantidad de elementos de la lista dada.
sacar n [] = []
sacar n (x:xs) =  if n == x
                    then xs
                    else x : sacar n xs --constante 

ordenar :: Ord a => [a] -> [a]      --O(n*(n*2)), siendo n la cantidad de elementos de la lista y 2 el número de operaciones lineales realizadas sobre estos.
                                    --Se realizan 2 operaciones lineales por cada elemento presente en la estructura.
ordenar [] = []
orderar xs =
    let m = minimo xs
        in m : ordenar (sacar m xs) --minimo y sacar son ambas lineales

--o sea, (minimo xs) : ordenar (sacar (minimo xs) xs)     
