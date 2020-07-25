module BinaryHeap(
	Heap,
	emptyH,
	isEmptyH,
	insertH,
	findMin,
	deleteMin,
	splitMin
 )
where

data Dir = Izq | Der
   deriving Show

nextPos [] = [Izq]
nextPos (Izq:ds) = Der : ds
nextPos (Der:ds) = Izq : nextPos ds

prevPos [] = [Izq]
prevPos (Der:ds) = Izq : ds
prevPos (Izq:ds) = Der : prevPos ds

esIzq Izq = True
esIzq Der = False

data Tree a =
      EmptyT
    | NodeT a (Tree a) (Tree a)
    deriving Show

data Heap a = H (Tree a) [Dir]
    deriving Show

-- Inv. Rep.:
-- 1) En la raiz del arbol está el mínimo
--    de todo el arbol
-- 2) El arbol está completo (sus niveles
--    están completos de izquierda a derecha)
-- 3) Si la lista está vacía el árbol también

-- Devuelve una heap vacia
-- Eficiencia: O(1)
emptyH :: Heap a
emptyH = H EmptyT []

-- Dice si una heap esta vacia
-- Eficiencia: O(1)
isEmptyH :: Heap a -> Bool
isEmptyH (H t ds) = isEmptyT t

isEmptyT EmptyT = True
isEmptyT _      = False

-- Agrega un elemento a la heap
-- Eficiencia: O(log n)
insertH :: Ord a => a -> Heap a -> Heap a
insertH x (H t ds) = H (insertT x t ds) (nextPos ds)

-- Proposito: 
-- Dado un arbol completo con el minimo
-- en la raiz, me devuelve un arbol completo
-- con el minimo en la raiz
-- Prec.:
-- 1) la lista de direcciones apunta a un EmptyT
-- 2) el arbol esta completo
-- Eficiencia: O(log n)
insertT :: Ord a => a -> Tree a -> [Dir] -> Tree a
insertT x EmptyT [] = NodeT x EmptyT EmptyT
insertT x (NodeT r ti td) (d:ds) =
	if esIzq d
	   then flotarIzq r (insertT x ti ds) td
	   else flotarDer r ti (insertT x td ds)

-- Proposito:
-- dado un elemento y un arbol con el minimo
-- en la raiz, devuelve un arbol con el
-- minimo en la raiz
flotarIzq r (NodeT x tii tid) td = 
	if x < r
	   then NodeT x (NodeT r tii tid) td
	   else NodeT r (NodeT x tii tid) td

-- Proposito:
-- dado un elemento y un arbol con el minimo
-- en la raiz, devuelve un arbol con el
-- minimo en la raiz
flotarDer r ti (NodeT x tdi tdd) = 
	if x < r
	   then NodeT x ti (NodeT r tdi tdd)
	   else NodeT r ti (NodeT x tdi tdd)

-- Encuentra el minimo elemento en la heap
-- Prec.: la heap tiene elementos
-- Eficiencia: O(1)
findMin :: Ord a => Heap a -> a
findMin (H t n) = root t

root (NodeT x ti td) = x
root EmptyT = error "no tiene elementos"

-- Borra el minimo elemento de la heap
-- Eficiencia: O(log n)
deleteMin :: Ord a => Heap a -> Heap a
deleteMin (H t n) = H
   (hundirT (splitAtPos t (prevPos n)))
   (prevPos n)

-- Eficiencia: O(log n)
hundirT :: Ord a => (a, Tree a) -> Tree a
hundirT (r, t) = hundirElem r t

-- Proposito:
-- Dado un elemento y un arbol completo y heap,
-- devuelve un arbol completo y heap
-- Eficiencia: O(log n)
hundirElem :: Ord a => a -> Tree a -> Tree a
hundirElem x t = undefined -- COMPLETAR

-- hundirElem r EmptyT = ...
-- hundirElem r (NodeT x ti td) = ...

-- Proposito:
-- dado un arbol y una lista de direcciones,
-- devuelve el elemento ubicado en la direccion
-- a la que se apunta y el arbol sin ese elemento
-- Eficiencia: O(log n)
splitAtPos :: Tree a -> [Dir] -> (a, Tree a)
splitAtPos = undefined -- COMPLETAR

-- Hace findMin y deleteMin
splitMin :: Ord a => Heap a -> (a, Heap a)
splitMin h = undefined
