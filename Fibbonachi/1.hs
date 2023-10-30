fibb n a b k | n==k = a
			 | k<n = fibb n (a+b) a (k+1)
fib 0 = 1
fib 1 = 1
fib n = fibb n 1 1 2