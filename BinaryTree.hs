data BTree a = NIL
               | Leaf a
			   | Node a (BTree a) (BTree a)
			   deriving (Eq, Show)

-- testet, ob ein Objekt x in einem geordneten Binärbaum enthalten ist			   
isElem x NIL   = False
isElem x (Leaf v) 
   | x == v    = True
   | x /= v    = False
isElem x (Node v b1 b2)
   | x == v    = True
   | x > v     = isElem x b2
   | x < v     = isElem x b1

-- liefert das grösste Element in einem geordneten Binärbaum
greatest NIL      = error "greatest NIL nicht definiert"
greatest (Leaf v) = v
greatest (Node v b1 b2) 
   | b2 == NIL    = v
   | otherwise    = greatest b2
    
-- liefert das kleinste Element in einem geordneten Binärbaum
smallest NIL      = error "smallest NIL nicht definiert"
smallest (Leaf v) = v
smallest (Node v b1 b2) 
   | b1 == NIL    = v
   | otherwise    = smallest b1

-- geordnetes Einfügen eines Elements x in einen GBB
insert x NIL  = Leaf x
insert x (Leaf v)
   | x >= v   = Node v NIL (Leaf x) 
   | x < v    = Node v (Leaf x) NIL
insert x (Node v b1 b2)
   | x >= v   = Node v b1 (insert x b2)
   | x < v    = Node v (insert x b1) b2

-- Löschen eines Elements x aus einem GBB
delete x NIL      = NIL
delete x (Leaf v)
   | x == v       = NIL
   | x /= v       = Leaf v
delete x (Node v b1 b2) 
   | x < v        = Node v (delete x b1) b2
   | x > v        = Node v b1 (delete x b2)   
   | b1 == NIL    = b2
   | b2 == NIL    = b1
   | otherwise    = Node w (delete w b1) b2
                    where w = greatest b1
   
-- geordnete lineare Ausgabe in einer Liste
linOut NIL            = []
linOut (Leaf v)       = [v]
linOut (Node v b1 b2) = (linOut b1) ++ [v] ++ (linOut b2) 

-- Höhe eines binären Baumes 
height NIL            = 0
height (Leaf v)       = 1
height (Node v b1 b2) = 1 + max (height b1) (height b2)
   
-- Testbäume
test1 = 
   Node 23 (Node 17 (Node 11 (Node 9 (Leaf 4) NIL) NIL) NIL) 
     	 (Node 45 (Leaf 39) (Leaf 51))

test2 = Node 10 (Node 6 (Leaf 4) (Leaf 8)) (Leaf 12)		 