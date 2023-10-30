kol_elemt p xs = foldr (\x n -> if p==x then n+1 else n ) 0 xs 

kol_elem p xs  = kol_elem1 p xs 0
kol_elem1 p xs n = foldr (\x (p,n) -> if p==x then (p,n+1) else (p,n)) (p,0)  xs



evqlid x = sqrt ( za2 x 0)
za2 x s = foldr (\x (s) -> (x*x+s)) (0) x

{-
del_dupl (x:xs) = del_dup1 (x:xs) 0
{-za3 x y p = foldl (\x (y,p) -> 	if  (p>=1)
								then        if (x == y) 
											then  (\x -> (x,p)) 
											else (\(y,p) x -> (y*0+x,p) x)
											
								else 		if (x == y) 
											then (\(y,p) -> x (y,p+1) )
											else (y,0) ) (y,0) x 
											

del_dup1 (x:xs) p = foldl (\x (p) ->if p==x 
									then (\x-> x:[])
									else (\x (p) -> (p*0+x) )) (p) (xs)  --- Здесь должна произойти замена p на x
									-}
									
							-}		
del_dup1 (x:xs) = foldl (\x (p)-> if x==p then p else x: foldl xs) (p) (xs)
		where p = x