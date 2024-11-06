import Set
import Mago
import EscuelaDeMagia

hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo        -- O(M * (log M + H log H))
 -- Propósito: Retorna todos los hechizos aprendidos por los magos.
 -- Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos escuela = hechizosDeEn (magos escuela) escuela

hechizosDeEn :: [Nombre] -> EscuelaDeMagia -> Set Hechizo  -- O(M * (log M + H log H))
hechizosDeEn []     escuela = emptyS
hechizosDeEn (n:ns) escuela = unionS (hechizosDe n escuela)
                                     (hechizosDeEn ns escuela)
  -- hechizosDe es O(log M)
  -- unionS es O(H log H), porque el tamaño máximo de un conjunto de hechizos es H
  -- Entre ambos, da O(log M + H log H)
  --  Como sucede para cada nombre de la lista, que son M, entonces da
  --   O(M * (log M + H log H))                                    

hayUnExperto :: EscuelaDeMagia -> Bool
 -- Propósito: Indica si existe un mago que sabe todos los hechizos
 --            enseñados por la escuela.
 -- Eficiencia: O(log M)
hayUnExperto escuela = let (m, newEscuela) = egresarUno escuela
                        in leFaltaAprender (nombre m) escuela == 0
  -- El costo es log M, porque egresarUno y leFaltaAprender ambas son de costo log M

egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
  -- Propósito: Devuelve un par con la lista de magos que saben todos
  --            los hechizos dados por la escuela y la escuela sin ellos.
  -- Eficiencia: O(M log M)
egresarExpertos escuela = if not (hayUnExperto escuela)
                           then ([], escuela)
                           else let (m , escuelaSinM)  = egresarUno escuela
                                    (ms, escuelaSinMs) = egresarExpertos escuelaSinM
                                 in (m : ms, escuelaSinMs)
   -- Las operaciones hayUnExperto, egresarUno son O(log M)
   -- Por cada mago (experto, que podrían ser todos...), usa esas dos funciones.
   -- Entonces, O(M log M)