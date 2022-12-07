import Prelude hiding (abs, seq, sum) 
type Complex a = (a,a)

real (a,b)      = a
imaginary (a,b) = b

sum (a,b) (c,d)  = (a+c,b+d)
prod (a,b) (c,d) = (a*c-b*d,a*d+b*c)
square (a,b)     = prod (a,b) (a,b)
scaling s (a,b)  = (s*a,s*b)
inverse (a,b)    = scaling (1/((abs (a,b)) * (abs (a,b))))
                   (conjugate (a,b))

abs (a,b)       = sqrt (a*a+b*b)
conjugate (a,b) = (a,-b)
angle (a,b)     = tan (b/a)
polar (a,b)     = (abs (a,b), angle (a,b))

seq (a,b) 1 = (a,b)
seq (a,b) n = sum (square (seq (a,b) (n-1)))((a,b))

mandelbrot (a,b) n = [seq (a,b) j | j <- [1..n]]