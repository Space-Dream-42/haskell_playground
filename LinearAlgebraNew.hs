-- vector with components of type a
type Vector a = [a] 

-- an element of a field
type Scalar a = a

-- returns the n-th component of a vector v  
vComp v n   = (!!) v (n-1)

-- returns the number of components of a vector v
vLength v
   | v == []           = 0
   | otherwise         = 1 + vLength(tail v)

-- returns the sum of two vectors v and w of the same vectorspace
vSum v w   = [(vComp v n) + (vComp w n)| n <- [1..(vLength v)]]

-- returns the product of a vector v with a scalar a
vsMult a v   = [a*(vComp v n)| n <- [1..(vLength v)]]

-- returns the sum of the components of a vector v 
compSum v 
   | v == []          = 0
   | otherwise        = head v + compSum(tail v)

-- returns the scalarproduct of two vectors v and w of the same vectorspace
sProd v w = compSum [(vComp v n) * (vComp w n)| n <- [1..(vLength v)]]

-- returns the norm a a vector v
vNorm v = sqrt (compSum [(vComp v n) * (vComp v n)| n <- [1..vLength v]])

-- sums a set of vectors
vsSum vectors 
-- | vectors == []         = NIL
   | otherwise             = vSum (head vectors) (vsSum (tail vectors))
   
   
  