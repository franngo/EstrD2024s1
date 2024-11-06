module Biblioteca (Bib, libro, juntar, pedirPrestado, devolver, libros, consulta, fueManipulado)
    where

{-
Somos usuarios de los TADS Set y Map
interfaz de Set = emptyS - addS - elemS (belongS) - sizeS - removeS - unionS - intersectionS - setToListS
interfaz de Map = emptyM - insertM - lookupM - removeM - keysM (domM)
-}    

data Bib = B [Libro]              (Set libro)       (Map Libro Fecha) (Set (Libro, Fecha))
             --todos los libros   los disponibles   los prestados     las manipulaciones -prestamo o devolución- indicando libro y fecha
{-INV.REP:
  *Sea B ls sl mlf slf
  *cada libro de ls debe encontrarse o bien en sl o bien como clave en una asociación de mlf
  *no puede haber un libro en sl o que sea clave en una asociación de mlf que no pertenezca a ls
  *Los libros que sean clave en una o más asociaciones de slf deben encontrarse o bien en sl o bien como clave en una asociación de mlf.
-}             

type Libro = Int
type Fecha = Int

data ResultadoConsulta = Inexistente | Disponible | Prestado Fecha

--Complejidad O(1) (describe biblioteca con UN SOLO libro)
libro :: Libro -> Bib
libro l = B [l] (addS l emptyS) emptyM emptyS

--PRECOND= Las bibliotecas no deben tener libros en común.
juntar :: Bib -> Bib -> Bib
juntar (B ls sl mlf slf) (B ls' sl' mlf' slf') = B (ls++ls') (unionS sl sl') (unionM mlf mlf' (keysM mlf')) (unionS slf slf')

--PRECOND: La lista dada debe ser una lista de claves del segundo map dado.
unionM :: (Map Libro Fecha) -> (Map Libro Fecha) -> [Libro] -> (Map Libro Fecha)
unionM mlf _    []     = mlf
unionM mlf mlf' (l:ls) = let f = fromJust(lookupM l mlf')
                         in insertM l f (unionM mlf mlf' ls)
 
--PRECOND= El libro debe estar disponible para ser prestado.
--Complejidad O(log n+log F), siendo n la totalidad de libros de la biblioteca dada y F la totalidad de fechas en que se dieron 
--manipulaciones de los mismos en esta. Esto es así ya que se ejecutan las operaciones removeS, la cual tiene un costo logarítmico sobre n
--en este caso, insertM, la cual también tiene un costo logarítmico sobre n en este caso, y addS, la cual, en este caso, tiene un costo 
--O(log n*f), que equivale a O(log n+log f). Además, también se ejecuta elemS, que también tiene costo logarítmico sobre n en 
--este caso.
pedirPrestado :: Bib -> Fecha -> Libro -> Bib
pedirPrestado (B ls sl mlf slf) f l = if(elemS l sl)
                                    then (B ls (removeS l sl) (insertM l f mlf) (addS (l,f) slf) )
                                    else (B ls sl mlf slf) --me cubre la precondición, pero esto asegura que no se describa algo inválido

--PRECOND= El libro debe estar prestado.
--Complejidad O(log n+log F), siendo n la totalidad de libros de la biblioteca dada y F la totalidad de fechas en que se dieron 
--manipulaciones de los mismos en esta. Esto es así ya que se ejecutan las operaciones addS, la cual tiene un costo logarítmico sobre n
--en este caso, removeM, la cual también tiene un costo logarítmico sobre n en este caso, y addS, la cual, en este caso, tiene un costo 
--O(log n*f), que equivale a O(log n+log f). Además, también se ejecuta lookupM, que también tiene costo logarítmico sobre n en 
--este caso.
devolver :: Bib -> Fecha -> Libro -> Bib
devolver (B ls sl mlf slf) f l = if(lookupM l mlf /= Nothing)
                                   then (B ls (addS l sl) (removeM l mlf) (addS (l,f) slf) )
                                   else (B ls sl mlf slf) --me cubre la precondición, pero esto asegura que no se describa algo inválido

--Complejidad O(1)
libros :: Bib -> [Libro]
libros (B ls _ _ _) = ls

--Complejidad O(log n), siendo n la totalidad de libros de la biblioteca dada. Esto es así ya que se ejecutan las operaciones elemS, la
--cual tiene un costo logarítmico sobre n en este caso, y lookupM, la cual también tiene un costo logarítmico sobre n en este caso.
consulta :: Bib -> Libro -> ResultadoConsulta
consulta (B _ sl mlf _) l = if(elemS l sl)
                                 then Disponible
                                 else case (lookupM l mlf) of
                                   Just f  -> Prestado f
                                   Nothing -> Inexistente

--Complejidad O(log n+log F), siendo n la totalidad de libros de la biblioteca dada y F la totalidad de fechas en que se dieron 
--manipulaciones de los mismos en esta. Esto es así ya que se ejecuta la operación elemS, la cual, en este caso, tiene un costo 
--O(log n + log F).
fueManipulado :: Bib -> Fecha -> Libro -> Bool
fueManipulado (B _ _ _ slf) f l = elemS (l,f) slf

--n representa la totalidad de libros de la biblioteca dada y F representa la totalidad de fechas en que se dieron manipulaciones de los 
--mismos en esta -o sea, fechas en que fue prestado o devuelto un libro-.

--slf tiene, en el peor de los casos, n*F elementos, ya que en ese peor caso, en todos los días en que se realizaron manipulaciones, se 
--manipularon TODOS los libros existentes en la biblioteca (o sea, se dieron n*F manipulaciones). Recorrer el set, entonces, tendría un 
--costo de O(log n*F), lo cual equivale a O(log n + log F).
