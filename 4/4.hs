z5' x y = z5 x y 0 0
z5 x y t r | y>x = x
		   | r==y = z5 x y (t+1) 0
		   | y==x = 1
		   | x< y * t = error "ПЕРЕБОР y * t"
		   | r<y =	if (y/=0) 
					then 
							if  x==y * t +r
							then r
							else 
									if x< y * t + r
									then z5 x y (t+1) 0
									else 	
											if x> y * t + r
											then  z5 x y t (r+1)
											else error "Строка "
							
					
					else 0
			
			