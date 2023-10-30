inc x | x==0 = 0
	  | x>0 = x+1
	  | otherwise = error "Arg >=0 !!!"

dec x | x>0 = x-1
	  | x == 0 = 0
	  | otherwise = error "Arg >=0 !!!"
	  
mns a b | (b==0) = a 
		| (b>0) = if (a > b) then mns (dec a) (dec b) else 0
		
pls a b | (b==0) = a
		| (a == 0) = b
		| (b>0)&&(a>0) = if (a > b) then pls (inc a) (dec b) else pls (dec a) (inc b)

mlt a b = mlt' a b 0
mlt' a b n | (b==0) = n     -- ????
		   | (b>0) = mlt' a (dec b) (pls n a)

max' a b = if (a < b) then b else a
min' a b = if (b < a) then b else a

koRecursCHas m k = cha m k 0   ---FIXED
koRecursCHas m 0 = 0
cha m k n  = (if ((m>=0)&&(k>=0)) 
			 then  				(if (k/=0)  
								then 
										    (if (m >= mlt k n) 
											then cha m k (n+1) 
											else n-1)  
								else 0) 
			 else error "ОШИБКА!!!")


koRecursCHas' m k = cha' m k 0
koRecursCHas' m 0 = 0
cha' m k n  = (if (k/=0)  
			  then 
						    (if (m >= k*n) 
							then cha' m k (n+1) 
							else n-1)  
			  else 0) 



{-
recursCHas m k = chat m k k
recursCHas m 0 = 0
chat m k k = 
-}
z5 x y = z55 x y 0 0
z55 x y t r | y>x = x
		   | r==y = z55 x y (t+1) 0
		   | y==x = 0
		   | x< y * t = error "ПЕРЕБОР y * t"
		   | r<y =	if (y/=0) 
					then 
							if  x==y * t +r
							then r
							else 
									if x< y * t + r
									then z55 x y (t+1) 0
									else 	
											if x> y * t + r
											then  z55 x y t (r+1)
											else error "Строка 66"
							
					
					else 0
			
			
			

z5'' x y =  if (y/=0) then x - ( y* (koRecursCHas' x y)) else 0

z6 x y = za6 x y 0
za6 x y t | y == 0 = error "0?"
		  |y /= 0 = if (t<=x)  
					then 
								if (t==x/y) 
								then True  
								else  za6 x y (t+1) 
					else False   
			

quotREm x y q   | (x==0) = (div x y, 0)
				| (x==y) = quotREm (x-y) y (q+1)
				| (x<y) = (q,x)
				| (x>y)= quotREm (x-y) y (q+1)

				
				
quotREm2 x y q  | (x-q*y>0) = (q,-(q*y-x))
				| (x-q*y<=0)= quotREm2 (x) y (q-1)
				
quotREm3 x y q  | (x-y*q>0) = (q, x)
				| (x-q*y<=0)= quotREm3 (x-y) y (q+1)

quotREm4 x y q  | (-x+(y*q)>0) = ((q+1),-((y*(q+1))-x))
				| (-x + (y*q) ==0) = (q,0)
				| ((q * y)-x<0)= quotREm4 (x) y (q-1)



quotRem' x y = quotRem'' x y 		-- Основная функция quotRem'
quotRem'' x y | x == 0 = (0,0)
			  | y == 0 = error "y /= 0!"
			  | x==y = (div x y,0)
			  | y == 1 = (x,0)
			  | x>0&&y>0&&x<y = (0,x)
			  | x>0&&y>0&&x>y = quotREm x y 0 
			  | x<0&&y>0 = quotREm2 x y 0
			  | (x<0)&&(y<0) = quotREm3 x y 0
			  | (x>0)&&(y<0) = quotREm4 (x) (y) (0)
			 
					
				


z7 x = za7 x 1 x 0
za7 x d t n| d<x&&t>=1 = if d*t==x then za7 x (d+1) (t-1) (n+1) else za7 x d (t-1) n
		   | t==0 = za7 x (d+1) x n
		   | d==x = n+1

z8 x = za8 x 1 x 0
za8 x d t n| d<x&&t>=1 = if d*t==x then za8 x (d+1) (t-1) (n+d) else za8 x d (t-1) n
		   | t==0 = za8 x (d+1) x n
		   | d==x = n+d
		   
z9 x = za9 x 
za9 x | x == 0 = error "0?"
	  | x+1==(z8 x) = True
	  | x+1 /= (z8 x)  = False


z10 x = za10 x 1 x 0
za10 x d t n| x == 1 = 0
			| z9 x == True = 1
			| (d<x&&t>=1) = if (d*t==x)&&(z9 d == True) then za10 x (d+1) (t-1) (n+1) else za10 x d (t-1) n
		    | t==0 = za10 x (d+1) x n
		    | d==x = n

z11 x y =  za11 x y
za11 x 0 = x
za11 x y = za11 (y)( rem x y)




z12 x y = za12 x y x
za12 x y n = if (z5 n x == 0 && z5 n y == 0) 
			 then n
			 else za12 x y (n+1)


--z13 m g  = za13 m g 1 0
--za13 m g x i | x>100 = error "зациклился"   --  z13 (\x -> mod x 13 == 0) (\x -> x*3) 1
	--	     | m(g(x)) == True = i
		--     | otherwise = za13 m g (x+1) (i+1)
