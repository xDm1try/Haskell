sumGeom b q n | n>0 = b + sumGeom (b*q) q (n-1)
			  |otherwise =0

z2 b q | abs (q) < 1 = b/(1-q)
	   |otherwise = error "Нельзя q>=1"
	   

funct b q e s i |((z2 b q) - s) < e = s
			  |otherwise = funct b q e (sumGeom b q (i+1)) (i+1)
--i =logBase (q) ((-1)*(((z3 b q e)*(1-q)/b)-1))


z3 b q e = funct b q e b 1

-------------------
--kolla::Int -> Int 
--kolla x   |(x == 1) =  x
--		  |(mod x 2 == 0) = kolla  (div x 2)
--		  |(mod x 2 == 1) = kolla (x*3+1)
e=0
g5 y = y + 1
z6 x = kolla x 0   -- ОСНОВНАЯ ФУНКЦИЯ
kolla:: Int -> Int -> Int
kolla x e | (x == 1) = e
				 | (mod x 2 == 0) = kolla  (div x 2) (e+1)  
				 | (mod x 2 == 1) = kolla (x*3+1)  (e+1)
