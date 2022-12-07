--type Statement = (Bool, Operator, Bool)

data Operator = not
              | and 
              | or 
			  | implication
			  | equivalence
			  deriving(Show)

{-
not a
   | a == True                = False
   | otherwise                = True

and a b 
   | a == True && b == True   = True
   | otherwise                = False
   
or a b 
   | a == True || b == True   = True
   | otherwise                = False
   
implication a b
   | a == True && b == True   = True
   | a == False               = True
   | otherwise                = False

equivalence a b
   | a == b                   = True
   | otherwise                = False   
-}
