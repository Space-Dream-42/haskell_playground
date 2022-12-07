import Prelude hiding (max, min, length)
-- 1. Haskell und Listen 
-- Def. Liste
--    data [a] = [] | a : [a], d.h [1,2,3] = 1 : [2,3] = 1 : 2 : 3

max a b 
   | a <= b   = b
   | a > b    = a
   
min a b 
   | a <= b   = a
   | a > b    = b

-- isElemList x []       = False
-- isElemList x [b] 
--    | x == b           = True
--    | x /= b           = False
-- isElemList x list
--    | x == head list   = True
--    | x /= head list   = isElemList x (tail list)

isElemList x list 
   | [y | y <- list, y == x] /= []   = True
   | otherwise                       = False    

-- Das zu löschende Element darf nicht doppelt auftauchen   
-- deleteList x []       = []
-- deleteList x [b] 
--    | x == b           = []
--    | x /= b           = [b]
-- deleteList x list
--    | x == head list   = tail list
--    | x /= head list   = (head list) : (deleteList x (tail list))

deleteList x list = [y | y <- list, y /= x] 
   
length list
   | list == []         = 0
   | otherwise          = 1 + length (tail list)   
   
maxList list
   | list == []         = error "not defined"
   | length list == 1   = head list
   | otherwise          = max (head list) (maxList (tail list))

minList list 
   | list == []         = error "not defined"
   | length list == 1   = head list
   | otherwise          = min (head list) (minList (tail list))
   
listSum list
   | list == []         = 0
   | otherwise          = (head list) + listSum (tail list)
   
mittelwert list  
   | list == []         = error "not defined"
   | otherwise          = (listSum list) / (length list)
   
sort list
   | list == []         = []
   | length list == 1   = list
   | otherwise          = [m] ++ (sort (deleteList m list))    
                         where m = minList list
						 
-- 2. (Geordnete) Binäre Bäume
-- Def. Binärer Baum
--  B ist genau dann ein binärer Baum, wenn
--        1) B = NIL (leerer Baum) 
--                oder
--        2) B = (lb,w,rb) (lb,rb binäre Bäume)
-- Def. Geordneter Binärbaum
--  Ein binärer Baum B heißt genau dann geordnet, wenn
--        1) B = NIL (leerer Baum)
--                oder 
--        2) B = (lb,w,rb), wobei Werte(lb) <(=) w < Werte(rb) 
--             lb,rb ebenfalls geordnete Binärbäume 

data BTree a = NIL   
               | Leaf a
			   | Node a (BTree a) (BTree a)
			   deriving (Eq, Show)

isElem x NIL            = False
isElem x (Leaf y)
   | x == y             = True
   | x /= y             = False
isElem x (Node y lb rb)   
   | x == y             = True
   | x < y              = isElem x rb
   | x > y              = isElem x lb
		
greatest NIL            = error "not defined"
greatest (Leaf y)		= y
greatest (Node y lb rb)
   | rb == NIL          = y
   | rb /= NIL          = greatest rb 
   
smallest NIL            = error "not defined"
smallest (Leaf y)		= y
smallest (Node y lb rb)
   | lb == NIL          = y
   | lb /= NIL          = smallest lb 
   
insert x NIL            = Leaf x
insert x (Leaf y)
   | x == y             = Leaf y
   | x < y              = Node y (Leaf x) NIL
   | x > y              = Node y NIL (Leaf x)
insert x (Node y lb rb)
   | x == y             = Node y lb rb
   | x < y              = Node y (insert x lb) rb
   | x > y              = Node y lb (insert x rb)

delete x NIL            = NIL
delete x (Leaf y)      
   | x == y             = NIL
   | x /= y             = Leaf y
delete x (Node y lb rb)
   | x < y              = Node y (delete x lb) rb 
   | x > y              = Node y lb (delete x rb)
   | lb == NIL          = rb
   | rb == NIL          = lb 
   | otherwise          = Node m (delete m lb) rb  
                         where m = greatest lb 

height NIL              = 0
height (Leaf y)         = 1	
height (Node y lb rb)	= 1 + max (height lb) (height rb) 				  

linOut NIL              = []
linOut (Leaf y)         = [y]
linOut (Node y lb rb)   = (linOut lb) ++ [y] ++ (linOut rb)

-- Testbäume
b1 = Node 6 (Leaf 4) (Leaf 7)
b2 = Node 11 NIL (Leaf 12)
b3 = Node 15 (Leaf 14) (Leaf 16)

b4 = Node 8 b1 (Leaf 9)
b5 = Node 13 b2 b3

tree = Node 10 b4 b5