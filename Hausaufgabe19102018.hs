{- Liefert alle durch k teilbaren Zahlen in [1..n] -}
choose :: Int -> Int -> [Int]
choose n k = [p|p <- [1..n], mod p k == 0]