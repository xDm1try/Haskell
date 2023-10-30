fib 0 = 0
fib 1 = 1
fib n = fibb n 1 1 0
fibb n k x y  | k<n = fibb n (k+1) y (x+y)
			  | k==n = y




			 