import Data.Char

-- Uebung 4.1.
myMax a b 
   | a >= b   = a
   | a < b    = b

-- Uebung 4.2.
derivative f x h  
   | h /= 0     = (f (x+h) - f x)/h  
   | h == 0     = error "h nicht gleich 0!"
   
-- Uebung 5.1.
duplizieren list = [2*x| x <- list]

-- Uebung 5.2.
entfernen n list = [x| x <- list, x  /= n]

-- Uebung 5.3. (fügt ein Element x hinter einem Element y einer Eq Liste list ein)
-- (für Listen in denen jedes Element verschieden ist)
einfuegenvor x y list = take (n-1) list ++ [x] ++ drop (n-1) list 
                        where n = getIndex y list
						
-- einfuegenvor x y list = take (n-1) list ++ [x] ++ einfuegenvor  x y (drop (n-1) list) 
--                        where n = getIndeces y list
-- einfuegenvor x y list 
--  | length (getIndeces y list) == 1    = take (n-1) list ++ [x] ++ drop (n-1) list                                        
--  | otherwise                          = take (n-1) list ++ [x] ++ einfuegen x y (drop (N-1) list)
--                                         where n = head (getIndeces y list)
--                                               N = getElem 
  
-- Uebung 5.4.
mittelwert list 
   | listLength list == 0    = error "Liste soll nicht leer sein!"
   | listLength list /= 0    = (listSum list) / (listLength list)
   
-- Uebung 5.5.
wertetab f list = [f x| x <- list]

-- Uebung 5.6.
gross s = [toUpper x| x <- s]

-- Uebung 6.1.
minmax a b c = (myMin a (myMin b c),myMax a (myMax b c))

-- Uebung 6.2.
dup x = (x,x) 

-- Uebung 6.3.
abstand (a,b) (c,d) = sqrt((a-c)^2 + (b-d)^2)

-- Uebung 6.4.
zwillinge n = [(p,q)| p <- [1..n], q <- [1..n], istZwilling (p,q) == True]

-- Uebung 6.5.
wertetab2 f list = [(x, f x)| x <- list]

-- Uebung 6.6. 
alter :: (Int,Int,Int) -> (Int, Int, Int) -> Int
alter (t1,m1,j1) (t2,m2,j2)
   | m2 > m1              = j2 - j1
   | m2 == m1 && t2 >= t1 = j2 - j1
   | otherwise            = j2 - j1 - 1

-- Hilfsfunktionen

-- liefert die Summe der Elemente einer numerischen Liste list
listSum list 
   | list == []      = 0
   | otherwise       = head list + listSum(tail list)

-- liefert die Anzahl von Elementen einer Liste list  
listLength list
   | list == []           = 0
   | otherwise            = 1 + listLength(tail list)
 
-- liefert von zwei Elementen a b einer geordneten Menge das minimale Element 
myMin a b 
   | a > b   = b
   | a <= b  = a  

-- liefert die Teiler einer natürlichen Zahl n
teiler :: Int -> [Int]
teiler n = [m| m <- [1..n], mod n m == 0]

-- entscheidet, ob eine natürliche Zahl n eine Primzahl ist
isPrime n 
   | n > 1 && teiler n == [1,n]  = True
   | otherwise                   = False

-- liefert die Primzahlen aus [1..n]
primes n = [p| p <- [1..n], isPrime p == True]   

-- entscheidet, ob eine natürliches Zahlenpaar (n,m) ein Primzahlzwilling ist  
istZwilling (n,m)
   | m == n+2 && isPrime n == True && isPrime m == True  = True
   | otherwise                                           = False   

-- liefert das Element zum Index j>=1 einer Liste list
getElem list j = (!!) list (j-1)
   
-- liefert den Index eines Elements a in einer Liste list (für Listen in denen jedes Element verschieden ist.)
getIndex a list = head [j| j <- [1..(length list)], getElem list j == a]
-- getIndeces a list = [j| j <- [1..(length list)], getElem list j == a]