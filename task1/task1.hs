module Foo where
(fi) x = (x*x)/(1+x)
se x = sqrt(3*x-(x^3))
th x = logBase 10 (x^2-21)
fo x = logBase 2 (logBase 3 (logBase 4 x))
fv x = sqrt(sin(2*x)) - sqrt ( sin (3*x))


leap x =  if ((mod x 4 == 0)||(mod x 400))&&(not then True else False

leap x = if ((mod x 4 == 0)||(mod x 400))&&(not (mod x 100)) then True else False