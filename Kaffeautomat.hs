-- parameters of a coffee machine (states ordered)
inputs  = ["K", "E", "R"]
states  = ["q0", "q1", "q2", "q3", "q4", "q5"]
initial = "q0"

-- the transition function
transition z e 
   | z /= "q5" && e == "K"               = getNext z states 
   | z /= "q4" && z /= "q5" && e == "E"  = getNext a states    
   | z == "q4" && e == "E"               = "q5"
   | z == "q5" && e /= "R"               = "q5"
   | e == "R"                            = "q0"
   | otherwise                           = error "non existing input or state!" 
                                           where a = getNext z states	

-- runs the coffee machine for a sequence of inputs	from a specific state 									   
runAt z list 
   | list        == []    = z
-- | length list == 1     = transition z (head list), not relevant.
   | otherwise            = runAt (transition z (head list)) (tail list)   

-- runs the coffee machine for a sequence of inputs	from the initial state   
run list = runAt initial list   
   
-- helpfunctions
getElem  list j  = (!!) list (j-1)  
getIndex a list  = head [j| j <- [1..(length list)], getElem list j == a]
getNext  a list  = getElem list (j + 1)
                   where j = getIndex a list 