import Prelude hiding(elem, min)

elem x list 
  | [y|y <- list, y == x] /= []    = True
  | otherwise                      = False
   

	