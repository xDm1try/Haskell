import qualified Data.Map as Map
import Data.Array

{-
{-# LANGUAGE FlexibleInstances #-}
instance {-# OVERLAPPING #-} Show String where show x = ['"'] ++ x ++ ['"']
-}

{-
freq [] = []
freq (x:xs) = (foldr (\x (a, n)  -> if a == x then (a,x+1) else (a, x)) (x,0) (x:xs)) : freq xs
-}

-- ==============--

trans2 person1 person2 sum |money person1  >= sum = [person1 {money = money person1  - sum}, person2 {money = money person2 + sum}] 
						   | otherwise = error "Недостаточно средств"

bank = Map.fromList [("Bob",100),("Mike",50)]

toMaybe:: Integer -> Maybe Integer
toMaybe a = Just a

sumMay:: Maybe Integer -> Maybe Integer -> Maybe Integer
sumMay (Just a) (Just b) = Just (a+b)

minMay:: Maybe Integer -> Maybe Integer -> Maybe Integer
minMay (Just a) (Just b) = Just (a-b)

trans p1 p2 sum bank|Map.lookup p1 bank >= toMaybe sum = Map.fromList [(p1,minMay (Map.lookup p1 bank)(toMaybe sum)),(p2,sumMay(Map.lookup p2 bank)(toMaybe sum))] 
					| otherwise = error "Недостаточно средств"

-- ==============--

listName = ["A", "B", "C", "D", "E", "F", "G", "K", "L", "M"] :: [String] -- русский не поставился
listID = [131, 132, 134, 135, 136, 137, 138, 139, 140, 141] :: [Int]
listGender = [F, F, F, M, M, M, F, F, F, F] :: [Gender]
listAge = [17, 18, 17, 19, 18, 19, 21, 19, 18, 20] :: [Int]
listFrends = [True, False, True, False, False, True, False, True, True, False] :: [Bool]

data Gender = F | M deriving (Show,Eq)

database = database1 listAge listFrends listGender listID listName
database1 _ _ _ [] _ = []
database1 (a:aa) (f:ff) (g:gg) (id:idx) (nm:nmm) = (id,(nm , g , a , f )) : database1 aa ff gg idx nmm

filterDB = filterDB1 database
filterDB1 [] = []
filterDB1 ((id,(nm,g,a,f)):db) = if g == M then (id, nm) : filterDB1 db else filterDB1 db

data Person = Person {
firstName :: String,
money :: Int } deriving (Show)

bill = Person {firstName = "Bill",
money = 100
}

mask = Person {firstName = "Elon",
money = 500
}

arI2 = array ((1,1),(2,3)) [((1,1),8), ((1,2),11),((1,3),4), ((2,1),3), ((2,2),2), ((2,3),4)]
arI :: Array (Int) (Double, Double)
arI = listArray (1,5) ([((-5.23),4.2),(2.456,13.234),(11241,211.2),((-1),0),(8.3,3.8)])


{-
sumAR arI = foldr (\x acc -> acc + (sqrt (x2-x1)+(y2-y1))) (0) (indices arI)	
					 let (x1,y1) = arI ! x
						 (x2,y2) = arI ! (x+1)  пишет: error: parse error on input `=' ?
-}

