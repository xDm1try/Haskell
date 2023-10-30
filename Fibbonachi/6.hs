fib 0 = 0
fib 1 = 1
fib n = fibb n 1 1 0
fibb n k x y  | k<n = fibb n (k+1) y (x+y)
			  | k==n = y

----------------------------------------------



solver y e l r | (abs (y ((l+r)/2))) < e = ((l+r)/2)
               | ((y (l) < 0)&&(y (r) > 0)) = if y ((l+r)/2) > 0 then solver y e l ((l+r)/2) else solver y e ((l+r)/2) r
		       | ((y (r) < 0)&&(y (l) > 0)) = if y ((l+r)/2) > 0 then solver y e ((l+r)/2) r else solver y e l ((l+r)/2)
			   | y (l) * y(r) > 0 = error "404"