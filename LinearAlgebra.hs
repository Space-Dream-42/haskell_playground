-- vectors with real components
type Vector = [Double]

-- returns the n-th component of a vector (n >= 1)
vComp :: Vector -> Int -> Double
vComp v n = (!!) v (n-1)  

-- sums to vectors of the same lenght
vSum :: Vector -> Vector -> Vector
vSum v w = [(vComp v n) + (vComp w n)| n <- [1..length v]]

-- multiplies a vector with a real scalar
sMultv :: Double -> Vector -> Vector
sMultv r v = [r * (vComp v n)| n <- [1..length v]] 

-- returns the scalar product of two vectors
sProd :: Vector -> Vector -> Double
sProd v w = sum [(vComp v n)*(vComp w n)| n <- [1..length v]]

-- gives the linear combination of a set of real scalars and a set of vectors
-- these sets must have the same cardinality
linComb :: [Double] -> [Vector] -> Vector
linComb scalars vectors = vsSum [ sMultv ((!!) scalars (n-1)) ((!!) vectors (n-1))| n <- [1..length vectors]] 

-- sums a set of vectors   
vsSum :: [Vector] -> Vector
vsSum vectors 
   | length vectors == 1   = head vectors
   | otherwise             = vSum (head vectors) (vsSum (tail vectors))  

-- returns the absolute value of a vector
vAbs :: Vector -> Double
vAbs v = sqrt (sum [(vComp v n)^2| n <- [1..length v]]) 

{-**************************************************************-}

-- matrices with real components
-- these vectors are the row vectors of the matrix, they must have the same lenght.
type Matrix = [Vector]

-- returns the number of rows in a matrix
numRows :: Matrix -> Int
numRows matrix = length matrix

-- returns the number of columns in a matrix
numCol :: Matrix -> Int
numCol matrix = length (head matrix)

-- returns the r-th row vector of a matrix
rowVec :: Matrix -> Int -> Vector
rowVec m r = (!!) m (r-1)
{-		    -}

-- returns the c-th column vector of a matrix
colVec :: Matrix -> Int -> Vector
colVec m c = [vComp (rowVec m r) c|r <- [1..numRows m]]

-- gives the matrix component in the r-th row and in the c-th column 
mComp :: Matrix -> Int -> Int -> Double
mComp m r c = vComp (rowVec m r) c 

-- sums two matrices
mSum :: Matrix -> Matrix -> Matrix
mSum a b = [vSum (rowVec a r) (rowVec b r)| r <- [1..numRows a]]

-- multiplies a matrix with a vector
vMultm :: Matrix -> Vector -> Vector
vMultm a v = [sProd (rowVec a r) v|r <- [1..numRows a]]

-- multiplies a matrix with a real scalar
sMultm :: Double -> Matrix -> Matrix
sMultm a m = [sMultv a (rowVec m r)| r <- [1..numRows m]] 

{- 
-- multiplies two matrices 
mProd :: Matrix -> Matrix -> Matrix
mProd a b = 





-}