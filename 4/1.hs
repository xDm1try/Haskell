-- [1,4,6] + [2,5,7] = [1,2,4,5,6,7]
--1 3 5
--2 4 6
f2 b = f1 0 b
f1 i (x:xs) = f1 (i+1) (xs)
f1 i [] = i

y1 (x:xs) = if (xs == []) then x else y1 (xs) -- <-- первое задание

z2 (x:xs) (y:ys)    |x<y = x:y: z2 (xs) (ys)  -- 2-е задание
					|y<x = y:x: z2 (xs) (ys)
					|y==x = x:y: z2 (xs) (ys) 
z2 (x:xs) []  = []
z2 [] (y:ys)  = [] 
z2 [] [] = []


b2 :: (a -> Bool) -> (a -> a) -> [a] -> [a]    -- 3-е задание
b2 pr f [] = []
b2 pr f (x:xs) | pr x == True = (f x) : b1 pr f xs
			   | pr x == False = x : b1 pr f xs
			   | otherwise = error "ОШИБКА"
