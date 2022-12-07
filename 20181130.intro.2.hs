-- in-gk (Q3)                                     23.11.2018
-- Hans-Carossa-Gymnasium
-- Dr. Rüßmann

-- TO DO!
-- Vervollständigen Sie die nachfolgenden (Funktions-)
-- Definitionen. Testen Sie Ihre Definitionen.
-- Wie erstellen Sie dazu 'effizient' einen Beispiel-Baum,
-- mit dem Sie die einzelnen Funktionen 'bequem' 
-- ausführen können?

-- import mit hiding von funktion(en) [Beispiel en passent] 
import Prelude hiding (max)


-- eigene definition der hidden function(s)
max a b 
	| a < b		= b
	| a >= b	= a

-- Datentyp: binary tree (over Type a)
-- insb. Leaf als nachfolgerfreies "Ende des Astes" 
-- (in einem Blatt = Leaf)	
data BTree a =	NIL
		| Leaf a
		| Node a (BTree a) (BTree a) 
		deriving (Eq, Show)

--Prüfung, ob ein Objekt im Baum ist
isElem x NIL	= False
isElem x (Leaf y)
	| x == y	= True
	| x /= y	= False
isElem x (Node y rb lb)
	| x == y	= True 
	| x  < y	= isElem x rb
	| x  > y	= isElem x lb
 	
	
--Größtes Element im Baum
greatest NIL 		= error "greatest NIL n. d."






-- Geordnetes Einfuegen (insert)
insert x NIL = Leaf x
insert x (Leaf y)
	| x <= y	= Node y (Leaf x) NIL
	| x  > y	= Node y NIL (Leaf x)
insert x (Node y rb lb)




--Kleinstes Element im Baum
smallest NIL 		= error "smallest NIL n. d."





	
-- Geordnete lineare Ausgabe in einer Liste
linOut NIL		= []
linOut (Leaf y)		= [y]
linOut (Node x rb lb)	= (linOut rb) ++ [x] ++(linOut lb)

testT = 
   Node 23 (Node 17 (Node 11 (Node 9 (Leaf 4) NIL) NIL) NIL) 
           (Node 45 (Leaf 39) (Leaf 51))
