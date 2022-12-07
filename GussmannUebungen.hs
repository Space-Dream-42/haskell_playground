import Data.Char

{- Uebung 4.1. -}
max :: (Float, Float) -> Float
max (a,b)
  | a >= b    = a
  | otherwise = b

{- Uebung 4.2. -}
derivative :: (Float -> Float) -> Float -> Float -> Float
derivative f x h = (f (x+h) - f x)/h

{- Uebung 5.1. -} --Nochmal neu in anderem Projekt!
duplizieren :: [a] -> [a]
duplizieren list = list ++ list

{- Uebung 5.2. -}
entfernen :: (Eq a) => a -> [a] -> [a]
entfernen n list = [x| x <- list, x /= n]

{- Uebung 5.3. -} {- Noch nicht vollständig! -}
einfuegenvor :: Int -> Int -> [Int] -> [Int]
einfuegenvor m n list = take (w-1) list ++ [m] ++ drop (w-1) list
                        where w = stellenElement list n 

{- Uebung 5.4. -}  --Nochmal neu in anderem Projekt!
mittelwert :: [Int] -> Int
mittelwert list
   | mod (length list) 2 /= 0   = stellenElement list (div (length list +1) 2)
   | otherwise                  = stellenElement list (div (length list) 2)
                                
{- Uebung 5.5. -}
wertetab :: (a -> b) -> [a] -> [b]
wertetab f list = [f x| x <- list]


{- Uebung 5.6. -}
gross :: String -> String
gross s = [toUpper c| c <- s]
    
{- Uebung 6.1. -}
minmax :: Float -> Float -> Float -> (Float, Float)
minmax a b c = (Main.min (mi,c),Main.max (ma,c))
               where mi = Main.min (a,b)
                     ma = Main.max (a,b)


{- Uebung 6.3. -}                  
abstand :: (Double, Double) -> (Double, Double) -> Double
abstand (a,b) (c,d) = sqrt ((a-c)^2 + (b-d)^2)
 
{- Uebung 6.2. -}
dup :: a -> (a,a)
dup n = (n,n) 

{- Uebung 6.4. -}
zwillinge :: Int -> [(Int,Int)]
zwillinge n = [(p,q)|p <- [1..n], q <- [1..n], istZwilling (p,q) == True]
 
{- Uebung 6.5. -}
wertetab2 :: (a -> b) -> [a] -> [(a,b)]
wertetab2 f list = zip list (wertetab f list)

{- Uebung 6.6. -}
alter :: (Int,Int,Int) -> (Int, Int, Int) -> Int
alter (t1,m1,j1) (t2,m2,j2)
   | m2 > m1              = j2 - j1
   | m2 == m1 && t2 >= t1 = j2 - j1
   | otherwise            = j2 - j1 - 1

 
{- Hilfsfunktion 1 (Liefert das n-te Element einer Liste)-}
stellenElement :: [a] -> Int -> a 
stellenElement list n = last(take n list)

{- Hilfsfunktion 2 (Liefert die Stelle an der sich ein Element in einer Liste befindet)-}
{- 
elementStelle :: (Eq a) => [a] -> a -> Int
elementStelle list n 
   | list /=[] && n == head list  = 1
   | otherwise                    = 1 + elementStelle ((entfernen (head list) list (elementStelle list n) - 1)  
-}

{- Hilfsfunktion 3 - (Liefert das kleinste Element von zweien) -}
min :: (Float, Float) -> Float
min (a,b)
  | a <= b    = a
  | otherwise = b
  
{- Hilfsfunktion 4 (Liefert die Teiler einer natürlichen Zahl)-}  
teiler :: Int -> [Int]
teiler n = [m| m <- [1..n], mod n m == 0]

{- Hilfsfunktion 5 (Entscheidet, ob eine natürliche Zahl prim ist.) -}
isPrime :: Int -> Bool
isPrime n 
   | n /= 1 && teiler n == [1,n] = True
   | otherwise                   = False
   
{- Hilfsfunktion 6 (Liefert die ersten n Primzahlen) -}
primes :: Int -> [Int]
primes n = [p| p <- [1..n], isPrime p == True]

{- Hilfsfunktion 7 (Entscheidet, ob ein nat. Zahlenpaar ein Primzahlzwilling ist) -}
istZwilling :: (Int,Int) -> Bool
istZwilling (n, m)
   | m == n+2 && isPrime n == True && isPrime m == True  = True
   | otherwise                = False   