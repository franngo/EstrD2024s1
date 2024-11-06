module EscuelaDeMagia(
    EscuelaDeMagia, fundarEscuela, estaVacia
                  , magos, registrar, hechizosDe
                  , leFaltaAprender, egresarUno, enseniar     
) where

import Mago
import Map
import Set
import PriorityQueue

data EscuelaDeMagia = EDM (Set Hechizo)         -- Todos los enseñados
                          (Map Nombre Mago)     -- Magos por nombre
                          (PriorityQueue Mago)  -- Magos por poder
  {- INV.REP.:
      * si un mago está en el Map, también debe estar en la PQ, y viceversa
         (y ser exactamente el mismo mago... -- mismos hechizos)
      * si cualquier mago tiene un hechizo, tiene que estar en el Set
      * en la PQ no hay dos magos con el mismo nombre
  -}

fundarEscuela   :: EscuelaDeMagia                              -- O(1)
estaVacia       :: EscuelaDeMagia -> Bool                      -- O(1)
magos           :: EscuelaDeMagia -> [Nombre]                  -- O(M)
registrar       :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia  -- O(log M)
hechizosDe      :: Nombre -> EscuelaDeMagia -> Set Hechizo     -- O(log M)   
leFaltaAprender :: Nombre -> EscuelaDeMagia -> Int             -- O(log M)
egresarUno      :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)    -- O(log M)
enseniar        :: Hechizo -> Nombre                           -- O(M log M
                    -> EscuelaDeMagia -> EscuelaDeMagia        --   + log H)


fundarEscuela = EDM emptyS emptyM emptyPQ

estaVacia (EDM _ _ p) = isEmptyPQ p
   -- ¿Vale usar (sizeS s == 0)?
   --   No. ¿Y si hubo magos y egresaron todos? ¡Va haber hechizos en una escuela vacía!
   -- ¿Vale usar (domM m == [])?
   --   No, porque quedaría costo O(M)
   -- ¿Necesito preguntar por el map?
   --   No, porque el invariante me garantiza que si una está vacía, la otra también.

magos (EDM _ mm _) = domM mm  -- keys m (con el nombre que puso Fede...)
   -- ¿Vale recorrer la PQ sacando los nombres de cada mago?
   --   No, porque quedaría costo O(M log M)

registrar n (EDM s mm p) = 
    case (lookupM n mm) of
      Nothing -> let m = crearM n
                  in EDM s (assocM n m mm) (insertPQ m p)
      Just _  -> EDM s mm p
    -- Debo buscarlo, porque si no violaría invariantes (y contrato)
    -- Debo agregarlo en ambos, para cumplir el inviarante
    -- No hace falta modificar el set, porque el nuevo mago no tiene hechizos...

hechizosDe n (EDM s mm p) = buscarHechizos n mm

buscarHechizos :: Nombre -> Map Nombre Mago -> Set Hechizo   -- O(log M)
buscarHechizos n mm = case (lookupM n mm) of
                        Nothing -> error "No estudia acá"
                        Just m  -> hechizos m
                      --ALTERNATIVA:
                      --let m = fromJust (lookupM n mm)                              
                      -- in hechizos m

leFaltaAprender n e@(EDM s mm _) = sizeS s - sizeS (hechizosDe n e)
    -- Vale usar funciones anteriores
    -- ALTERNATIVA: (para evitar el doble pattern matching) funciones auxiliares
    --   sizeS s - sizeS (buscarHechizos n mm)

egresarUno (EDM s mm p) = 
    let m = findMinPQ p
     in (m, EDM s (deleteM (nombre m) mm) (deleteMinPQ p)) 
     -- Se borra de los dos lados para mantener el invariante

enseniar h n (EDM s mm p) = 
    case (lookupM n mm) of
      Nothing -> error "No estudia acá"
      Just m  -> let newM = aprender h m
                  in EDM (addS h s) (assocM n newM mm)
                         (modificarPQ newM p)
    -- ¿Por qué es seguro usar modificarPQ, y no da error?
    --   Por el invariante, si m está en mm, también está en p                         

modificarPQ :: Mago -> PriorityQueue Mago 
                    -> PriorityQueue Mago        -- O(M log M)
-- PRECOND: tiene que haber un mago con el mismo nombre que el mago dado en la cola                    
modificarPQ m p = let mmp = findMinPQ p
                   in if (m == mmp)
                       then insertPQ m (deleteMinPQ p)
                       else insertPQ mmp (modificarPQ m (deleteMinPQ p))
