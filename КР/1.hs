import Prelude hiding (filter)
import Data.Map hiding (foldl)
import Data.List
import Control.Monad
import Data.Char

lst = [ intTo x | x<-[0,1..]]
intTo x =  (intto3sys x 3)
intto3sys x s | x<s = [str !! x]
            | otherwise = ((intto3sys ((x `div` s)-1) s)) ++ [str !! (x `mod` s)]
              where str="abc"
			  
			  
data Logger a = Logger [String] a deriving (Show, Eq)
instance Functor Logger where
fmap f (Logger l a) = Logger l (f a)
instance Monad Logger where
	return x = Logger [] x
	Logger la a >>= f = Logger (la++lb) b
	 where Logger lb b = f a
instance Applicative Logger 
	where
		pure = return
		(<*>) = ap
	
msg :: String -> Logger ()
msg s = Logger [s] ()
inc a = a+1

countAndLog pr list = countAndLogz pr list 0
countAndLogz pr [] n = return (0)
countAndLogz pr (x:xs) n = if pr x 
							then do 
									msg (show x)
									t <- countAndLogz pr xs (n)
									return (inc t)
							else countAndLogz pr xs n




t = fromList [(21, Nd 12 34), (12, Nd 51 26),(51, Lf), (26, Lf), (34, Nd 17 30), (17, Lf),(30, Nd 73 27), (73, Lf), (27, Lf)]

data ATree a = ALeaf a | ABranch (ATree a) a (ATree a) deriving (Show, Read, Eq)
data Tr = Nd Integer Integer | Lf deriving (Show, Read, Eq)

root t = find' (sorted t)
sorted t = sort (listnodes t)
listnodes t = listallleaf (toList t) []
listallleaf [] list = list
listallleaf ((x, Lf):xs) list = x: (listallleaf xs list)
listallleaf ((x, Nd l r):xs) list = x : l : r : (listallleaf xs list)
find' (x:xs) = 
				if x == head (xs) 
				then 
					find' (tail (xs)) 
				else 
					 (x)
					
					
findnode n ((x, Lf):xs) = if n == x then (x, Lf) else findnode n xs
findnode n ((x, Nd l r):xs) = if n == x then (x, Nd l r) else findnode n xs

makeTree (x, Nd l r) t = ABranch (makeTree (findnode l (toList t)) t) x (makeTree (findnode r (toList t)) t)
makeTree (x, Lf) t = ALeaf x


 
restoreTree :: Map Integer Tr -> ATree Integer
restoreTree t = makeTree (x, Nd l r) t
					where (x, Nd l r) = findnode (root t) (toList t)
					 




{-
count :: Eq a => a -> State [(a,Integer)] ()
count x = do 
			y <- get 
			
			if x == y 
				then do 
-}				
			