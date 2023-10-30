import Prelude hiding ((!!),init,reverse,(++),cycle,take,elem)

(!!) :: [a] ->Int-> a
(!!) [] _ = error "lenght list < idx"
(!!) (x:xs) idx | idx < 0 = error "int >= 0"
				| idx == 0 = x
				| otherwise = (!!) (xs) (idx-1)
				

				
				
init :: [a] -> [a]
init [] = error "[]!"
init (x:xs) | null xs = []
			| otherwise = x: init xs
			
			
			
(++)::[a] -> [a] -> [a]
(++) [] y = y
(++) a [] = a
(++) (x:xs) (y:ys) | null (xs)/= True&& null (ys) /= True = x: (++) xs (y:ys)
				   | null (xs)==True && null (ys) /= True =x: y: (++) xs ys
				   | null (xs)==True && null (ys) == True = []
				

cycle (x:xs) = cycle1 (x:xs) (x:xs)
cycle1 (x:xs) (y:ys) | null ys =y: cycle1 (x:xs) (x:xs)
					 | otherwise =y: (cycle1 (x:xs) (ys))


-- take1 как в примере
-- take0 как в условии

take0 :: Int -> [a] -> [a]
take0 _ [] = []
take0 n (x:xs) | n > 0 = x: take0 (n-1) xs
			  | n == 0 = x: []
			  | n < 0 = error "INT>0"
			

take1 :: Int -> [a] -> [a]			
take1 _ [] = []
take1 n (x:xs) | n > 1 = x: take1 (n-1) xs
			   | n == 1 = x: []
			   | n < 1 = error "INT>0"
			  
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : (tails xs)	  



lenght (x:xs) = lenght1 0 (x:xs)
lenght1 n [] = n
lenght1 n (x:xs) = lenght1 (n+1) (xs)

inits [] = [[]]
inits (x:xs) = [] : (inits1 0 0 (x:xs))	
inits1 n l (x:xs)|n < l = (take0 (n) (x:xs)) : (inits1 (n+1) l (x:xs))
				 |n == l = []
			where l = (lenght (x:xs))


elem a [] = False
elem a (x:xs) | a /= x = elem a xs
			  | a == x = True
			  
nub [] = []
nub (x:xs) = if elem x (xs)
				then nub (xs)
				else x: nub (xs)
				
updElmBy :: [a] -> Integer -> a -> [a]
updElmBy [] n a = error "[]"
updElmBy (x:xs) 0 a =a: xs 
updElmBy (x:xs) n a |n>=0 =x: (updElmBy (xs) (n-1) a) 
					|otherwise = error "idx >= 0"
					
{-			
tr [] a b = error "Ошибка с idx"
tr (x:xs) a b | a<0 || b<0 = error "idx >=0"
			  | otherwise = trs (x:xs) a b
tr1 (x:xs) a b | a == 0 = x
			   | otherwise = tr1 (xs) (a-1) b
tr2 (x:xs) a b | b == 0 = x
			   | otherwise = tr2 (xs) a (b-1)
trss (x:xs) a b = trs (x:xs) a b (b-a)
trs (x:xs) a b  k
-}
{-
permutations :: [a] -> [[a]]
permutations (x:xs) = 
-}

cubsumR xs = foldr (\x acc -> x*x*x + acc) 0 xs 

cubsumL xs = foldl (\acc x -> x*x*x + acc) 0 xs 

fact:: Int -> Int
fact 0 = 1
fact x = x * (fact (x-1))

factDB:: Double -> Double
factDB 0 = 1
factDB x = x * (factDB (x-1))


expT:: Double -> Double -> Double
expT x 0 = 1 
expT x n = ((x ** n)/(factDB (n))) + (expT (x) (n-1)) 


howmany a xs = b
		where (b, n) = foldl (\(acc,a) x -> if a == x then (acc+1, a) else (acc, a)) (0,a) xs
		



intersperse a [] = []
intersperse a (x:xs) = x: (intersperse1 (a) (xs))
intersperse1 a (x:xs)| null xs  = a: x : []
					 | otherwise = a:  x: (intersperse1 (a) (xs))

