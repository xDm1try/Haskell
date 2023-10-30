kol_elem p xs  = kol_elem1 p xs 0
kol_elem1 p xs n = foldr (\x (p,n) -> if p==x then (p,n+1) else (p,n)) (p,0)  xs



evqlid x = sqrt ( za2 x 0)
za2 x s = foldr (\x (s) -> (x*x+s)) (0) x


del_dup (x:xs) = del_dup1 (x:xs) []
{-za3 x y p = foldl (\x (y,p) -> 	if  (p>=1)
								then        if (x == y) 
											then  (\x -> (x,p)) 
											else (\(y,p) x -> (y*0+x,p) x)
											
								else 		if (x == y) 
											then (\(y,p) -> x (y,p+1) )
											else (y,0) ) (y,0) x 
											
-}
del_dup1 (x:xs) p  = foldl (\x (p) -> if	(filter (==x) p == []) 
									then  x:p
									else   x ) (p) (x:xs) 