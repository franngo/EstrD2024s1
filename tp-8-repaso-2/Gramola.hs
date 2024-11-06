import Resumen --tipo con el que, x ej, podemos guardar todos los votos a una determinada canción y obtener info en base a esos votos
--operaciones útiles: nuevoR, agR, cantidadR, maxR y promedioR (todas O(1))

data Gramola = [(String, Resumen)] --lista de anteriores (está invertida para mayor eficiencia)
               String              --canción actual
               Resumen             --resumen de canción actual
               [(String, Resumen)] --lista de siguientes
               String              --canción más votada
               Int                 --cantidad de votos de la más votada
{-INV REP.:
    *Sea G as ac rac ss mv cmv
    *mv es el nombre de la canción que más veces fue votada de la gramola, por lo que, además, es una canción perteneciente a esta (es ac o 
    pertenece a as o ss)
    *cmv equivale a la cantidad de veces que fue votada la canción más votada de la gramola, por lo que debe equivaler a la cantidad de 
    votos que recibió mv
-}

--PRECOND= La lista no puede ser vacía.
nuevaG :: [String] -> Gramola     
nuevaG (c:cs) = G [] c nuevoR (paresG cs) c 0

--[SUBTAREA]
paresG :: [String] -> [(String, Resumen)]
paresG []     = []
paresG (c:cs) = (c, nuevoR) : pares cs

--Complejidad O(1)
anteriorG :: Gramola -> Gramola --op total!
anteriorG (G [] ac rac ss mv cmv) = (G [] ac rac ss mv cmv)
anteriorG (G as ac rac ss mv cmv) = let (nac,nrac) = head as
                                    in (G (tail as) nac nrac ((ac,rac):ss) mv cmv)

--Complejidad O(1)
siguienteG :: Gramola -> Gramola --op total!
siguienteG (G as ac rac [] mv cmv) = (G as ac rac [] mv cmv)
siguienteG (G as ac rac ss mv cmv) = let (nac,nrac) = head ss
                                     in (G ((ac,rac):as) nac nrac (tail ss) mv cmv)

--Complejidad O(1)
cancionG :: Gramola -> String
cancionG (G as ac rac ss mv cmv) = ac

--Complejidad O(1)
votarG :: Int -> Gramola -> Gramola
votarG punt (G as ac rac ss mv cmv) = let nrac = agR punt rac
                                      in (G as ac nrac ss (cambioMV (cantidadR nrac) cmv ac mv) (cambioCMV (cantidadR nrac) cmv))

--Complejidad O(1) [SUBTAREA]
cambioMV :: Int -> Int -> String -> String -> String
cambioMV cac cmv ac mv = if(cac>cmv)
                           then ac
                           else mv

--Complejidad O(1) [SUBTAREA]
cambioCMV :: Int -> Int
cambioCMV cac cmv = if(cac>cmv)
                      then cac
                      else cmv

--Complejidad O(1)
votarGV2 :: Int -> Gramola -> Gramola
votarGV2 punt (G as ac rac ss mv cmv) = if(cantidadR rac==cmv)
                                          then (G as ac (agR punt rac) ss ac (cmv+1))
                                          else (G as ac (agR punt rac) ss mv cmv)

--Complejidad O(1)
--OBSERVACIÓN: Si nunca fue votado, devuelve 0.
puntajeG:: Gramola -> Int
puntajeG (G as ac rac ss mv cmv) = if(cantidadR rac==0)
                                   then 0
                                   else promedioR rac

--Complejidad O(1)
temaMasVotadoG :: Gramola -> String
temaMasVotadoG (G as ac rac ss mv cmv) = mv