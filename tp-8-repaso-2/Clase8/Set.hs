module Set (Set, emptyS, addS, belongs, unionS
               , removeS, set2list, sizeS) where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

data Set a  = S (Tree a) Int
  {- INV.REP.: en (S t n), t cumple ser un BST y n es el sizeT t -}

emptyS   :: Set a  -- O(1)
emptyS = S EmptyT 0

addS     :: Ord a => a -> Set a -> Set a  -- O(n) (o O(log n) en promedio)
addS x (S t n) = armarS (insertarBST x (t,n))

belongs  :: Ord a => a -> Set a -> Bool   -- O(n) (o O(log n) en promedio)
belongs x (S t _) = buscarBST x t

unionS :: Ord a => Set a -> Set a -> Set a  -- O(n^2) (o O(n log n) en promedio)
unionS (S t1 _) (S t2 n2) = armarS (agregarTodos (inorder t1) t2 n2)

removeS  :: Ord a => a -> Set a -> Set a
removeS x (S t n) = armarS (borrarBST x (t,n))

set2list :: Ord a => Set a -> [a]  -- O(n^2) o O(n) según corresponda...
set2list (S t _) = inorder t  -- Consecuencia del Inv.Rep.: ¡la lista está ordenada!

sizeS :: Set a -> Int      -- O(1)
sizeS (S _ n) = n

-- ==========================
-- Auxiliares
-- ==========================
armarS (t, n) = S t n

buscarBST :: Ord a => a -> Tree a -> Bool  -- O(n), en peor caso
                                           -- pero es O(log n) en promedio
  -- PRECOND: el árbol es BST
buscarBST x EmptyT          = False
buscarBST x (NodeT y ti td) =
    if (x == y)      then True
     else if (x < y) then buscarBST x ti
                     else buscarBST x td

insertarBST :: Ord a => a -> (Tree a, Int) -> (Tree a, Int)  -- O(n), en peor caso
                                                             -- pero es O(log n) en promedio
  -- PRECOND: el árbol es BST
insertarBST x (EmptyT       , n) = (NodeT x EmptyT EmptyT, n+1)
insertarBST x (NodeT y ti td, n) =
    if (x==y)        then (NodeT x ti td, n)
     else if (x < y) then armarNodeIzq y (insertarBST x (ti, n)) td
                     else armarNodeDer y ti (insertarBST x (td, n))

armarNodeIzq y (ti', n') td = (NodeT y ti' td, n')
armarNodeDer y ti (td', n') = (NodeT y ti td', n')

borrarBST :: Ord a => a -> (Tree a, Int) -> (Tree a, Int)
  -- PRECOND: el árbol es BST
borrarBST _ (EmptyT       , n) = (EmptyT, n)
borrarBST x (NodeT y ti td, n) =
    if (x==y)        then (rearmarBST ti td, n-1)
     else if (x < y) then armarNodeIzq y (borrarBST x (ti, n)) td
                     else armarNodeDer y ti (borrarBST x (td, n))

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
  -- PRECOND: los dos árboles son BSTs
rearmarBST EmptyT td = td
rearmarBST ti     td = let (m, ti') = splitMaxBST ti
                        in NodeT m ti' td

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
  -- PRECOND: el árbol es BST, y NO está vacío
splitMaxBST (NodeT x ti EmptyT) = (x, ti)  
splitMaxBST (NodeT x ti td)     = let (m, td') = splitMaxBST td
                                   in (m, NodeT x ti td')

inorder :: Tree a -> [a]    -- O(n^2), si (++) es O(n)
                            -- pero O(n), si (++) es O(1)
inorder EmptyT          = []
inorder (NodeT x ti td) = inorder ti ++ [x] ++ inorder td

agregarTodos :: Ord a => [a] -> Tree a -> Int -> (Tree a, Int)
agregarTodos []     t n = (t,n)
agregarTodos (x:xs) t n = insertarBST x (agregarTodos xs t n)
