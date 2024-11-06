import Biblioteca

{-
INTERFAZ DE BIBLIOTECA=
type Libro = Int
type Fecha = Int

data ResultadoConsulta = Inexistente | Disponible | Prestado Fecha

--Complejidad O(1) (describe biblioteca con UN SOLO libro)
libro :: Libro -> Bib

--PRECOND= Las bibliotecas no deben tener libros en común.
juntar :: Bib -> Bib -> Bib

--PRECOND= El libro debe estar disponible para ser prestado.
--Complejidad O(log n+log F)
pedirPrestado :: Bib -> Fecha -> Libro -> Bib

--PRECOND= El libro debe estar prestado.
--Complejidad O(log n+log F)
devolver :: Bib -> Fecha -> Libro -> Bib

--Complejidad O(1)
libros :: Bib -> [Libro]

--Complejidad O(log n)
consulta :: Bib -> Libro -> ResultadoConsulta

--Complejidad O(log n+log F)
fueManipulado :: Bib -> Fecha -> Libro -> Bool

*n representa la totalidad de libros de la biblioteca dada y F representa la totalidad de fechas en las que se dieron movimientos 
-fue prestado o devuelto libro- en la misma.
-}

--Complejidad O(n (log n + log F)). Esto es así ya que se ejecuta la operación devolverTodos', la cual tiene un costo de O(n (log n +log f)),
--además de la operación libros, cuyo costo es constante.
--(tengo que devolver AQUELLOS QUE SE ENCUENTRAN PRESTADOS!)
devolverTodos :: Fecha -> Bib -> Bib 
devolverTodos f b = devolverTodos' b f (libros b)

--Complejidad O(n (log n + log F)). Esto es así ya que, en el peor caso, se ejecutan en n ocasiones las operaciones devolver, la cual tiene
--costo O(log n+log f), y consulta, la cual tiene costo logarítmico sobre n. Además, en cada ocasión se hace uso de la estructura case of, 
--pero su costo es constante y no afecta al cálculo de la eficiencia.
devolverTodos' :: Bib -> Fecha -> [Libro] -> Bib
devolverTodos' b _ []     = b
devolverTodos' b f (l:ls) = case (consulta b l) of
                              Inexistente -> devolverTodos' b f ls
                              Disponible  -> devolverTodos' b f ls
                              Prestado n  -> devolverTodos' (devolver b f l) f ls

O SINO           

devolverTodos'V2 :: Bib -> Fecha -> [Libro] -> Bib
devolverTodos'V2 b _ []     = b
devolverTodos'V2 b f (l:ls) = case (consulta b l) of
                              Inexistente -> devolverTodos' b f ls
                              Disponible  -> devolverTodos' b f ls
                              Prestado n  -> devolver (devolverTodos' b f ls) f l