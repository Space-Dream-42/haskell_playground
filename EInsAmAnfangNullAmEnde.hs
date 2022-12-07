-- Der Automat A akzeptiert Wörter über dem Aphabet {0,1}, die mit 
-- 1 beginnen und mit 0 enden
states  = ["q0","q1","q2","q3"]
inputs  = [0,1]
initial = "q0"
finals  = ["q2"]

-- Die Übergangsfunktion von A 
transition "q0" 0 = "q3"
transition "q0" 1 = "q1"

transition "q1" 0 = "q2"
transition "q1" 1 = "q1"

transition "q2" 0 = "q2"
transition "q2" 1 = "q1"

transition "q3" 0 = "q3"
transition "q3" 1 = "q3"

-- führt die Zeichfolge input nacheinander in einem Zustand x aus
runAt x input
   | input == []         = x
   | length input == 1   = transition x (head input)
   | otherwise           = runAt (transition x (head input)) (tail input)   

-- führt die Zeichenfolge input nacheinander im Startzustand von A aus   
run input = runAt initial input

-- entscheidet, ob ein Wort w Teil der Sprache von A ist
dfa w
   | elem (run w) finals   = True
   | otherwise             = False