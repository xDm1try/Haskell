import Data.Array
{-
import qualified Data.Map as M
import Data.Map
-}


data Number = Finite Integer | Infinite deriving (Show, Eq)
instance Ord Number
	where
		_ <= Infinite = True
		Infinite <= _ = False
		(Finite x) <= (Finite y) = x <= y

arI :: Array (Int,Int) Int
arI = array ((1,1),(2,3)) [((1,1),8), ((1,2),11),((1,3),4), ((2,1),3), ((2,2),2), ((2,3),4)]
	
sumArray = foldr1 (+) arI
sumArray2 arI = foldl (\s x -> s + (arI ! x)) 0 (indices arI)
sumAR arI = foldr (\(x,y) s -> if (x+y) == (arI ! (x,y)) then arI ! (x,y):s else s) [] (indices arI)
maxARR arI = foldl1 (\x s -> if (arI ! x) > (arI ! s) then x else s)  (indices arI)


{-
vocabulary = (M.fromList [("Bob",3470),("Jane",2130),("Lisa",9448)])

winner :: M.Map String Integer -> String -> String -> String
winner vocabulary plr1 plr2 = if (M.lookup plr1 vocabulary) >= (M.lookup plr2 vocabulary) then plr1 else plr2
-}