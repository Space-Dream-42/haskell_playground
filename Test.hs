fak :: Int -> Int    
fak 1 = 1            
fak n = n * fak (n-1) 

fak2 :: Int -> Int
fak2 n
  | n == 1    = 1 
  | otherwise = n * fak2 (n-1) 











