import Data.List
import Data.Char(isDigit)

data PointD = PointD Double Double
data PointI = PointI Int Int

findFirstDigit:: [Char] -> Maybe Char
findFirstDigit [] = Nothing
findFirstDigit (x:xs) | isDigit x = Just x
					  | otherwise = findFirstDigit xs
					  

listToMaybe:: [a] -> Maybe a

listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

shiftI dx dy (PointI x y) = PointI (x+dx) (y + dy)
shiftD dx dy (PointI x y) = PointI (x+dx) (y + dy)

data Point a = Point a a deriving Show

shift dx dy (Point x y) = Point (x + dx) (y + dy)

distanceD (PointD x y) (PointD a b) =  sqrt((x-a)^2 + (y-b)^2)
 
manhDistance (PointI x y) (PointI a b) =  abs(x-a) + abs(y-b)

{-
maybeToList:: Maybe a -> [a]
maybeToList Nothing = []
maybeToList Just x = (x:_)
-}
myHead:: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x

myTail:: [a] -> Maybe [a]
myTail [] = Nothing
myTail (x:xs) = Just xs

data List a = Nil | Cons a (List a) deriving Show

fromList:: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

data Nat = Zero | Suc Nat deriving Show

fromNat Zero = 0
fromNat (Suc n) = fromNat n +1

c = (Suc(Suc(Suc Zero)))
a = (Suc Zero)

toNat::Int-> Nat
toNat x |x<0 = error "<0!"
		|x==0 = Zero
		|otherwise = Suc (toNat (x-1))

sumNat x y = (fromNat x) - (fromNat y)
sumNat1 x Zero = x
sumNat1 x y = (sumNat1 (x) (fromNat y))
			