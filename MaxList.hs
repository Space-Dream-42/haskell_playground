myMax a b 
	| a >= b    = a
	| a < b     = b	
	
maxList []  = error "MaxList der leeren Liste nicht definiert!"
maxList [a] = head [a]
maxList x   = myMax (head  x) (maxList(tail x))
 