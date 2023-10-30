import Control.Monad.Writer
import Control.Monad.State.Lazy
import Data.Char(intToDigit )
import Data.Either


data ATree a = ALeaf a | ABranch (ATree a) a (ATree a) deriving (Show, Read)

dTree :: ATree Double
-- dTree = ABranch (ABranch (ALeaf 21) 2.2 (ABranch (ABranch (ALeaf 62) 0 (ALeaf 66)) 22 (ALeaf 46))) 1.1 (ABranch (ALeaf 34) 3.3 (ALeaf 35))
dTree = ABranch (ABranch (ALeaf 21) 0 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46))) 1.1 (ABranch (ALeaf 34) 3.3 (ALeaf 35))


--1--

divWithTreeM :: Double -> ATree Double -> Maybe Double
divWithTreeM num tree = if dM tree == Nothing
						then Nothing
						else do
								d <- dM tree
								return (num/d)

dM :: ATree Double -> Maybe Double
dM (ALeaf x) = if x == 0 then Nothing else return x
dM (ABranch l d r) =if  d == 0 
						then Nothing
						else 
							do
								le <- dM (l)
								re <- dM (r)
								return (d * le * re)

--2--
idTree :: ATree (Int,Double)
idTree =
    ABranch
       (
         ABranch
            (ALeaf (4,21))
              (2,2.2)
            (
              ABranch
                 (
                   ABranch
                     (ALeaf (100,62))
                       (7,3)
                     (ALeaf (200,66))
                 )
                     (5,22)
                  (ALeaf (203,46))
            )

       )
             (1,1.1)
     (
       ABranch
         (ALeaf (301,4))
            (4,0)
         (ALeaf (307,6))
     )
divWithTreeE :: Double -> ATree (Int,Double) -> Either String Double
divWithTreeE num tree = if isLeft $ dME tree 
						then dME tree
						else do
								r <- dME tree
								Right(num/r)	
digs :: Int -> [Int]
digs 0 = []
digs x = (mod x 10 : digs (div x 10))
myStr s = map (intToDigit) $ reverse $ digs s
dME (ALeaf (id,db)) = if db == 0 then Left ("smth. wrong with ID: "++myStr (id)) else Right db
dME (ABranch l (id,db) r) = if  db == 0 
							then Left ("smth. wrong with ID: "++myStr (id))
							else 
								do
									le <- dME (l)
									re <- dME (r)
									return (db * le * re)
--3--
sumWithTree (ABranch l (id,db) r) = runWriter $ sumWithTreeE (ABranch l (id,db) r)
sumWithTreeE :: ATree (Int,Double) -> Writer String Double
sumWithTreeE (ALeaf (id,db)) = do
								tell ("ID: " ++ myStr (id) ++ " - " ) 
								return db
sumWithTreeE (ABranch l (id,db) r) = do
								le <- sumWithTreeE (l)
								tell  ("ID: " ++ myStr (id) ++ " - ")
								re <- sumWithTreeE (r)
								return (db + le + re)

--4--
{-
--numTree :: ATree Double -> ATree (Int,Double)
numTree (ABranch l d r) = numTree' (ABranch l d r) 0
							
numTree' (ALeaf d) n = do
						return (ALeaf (d,n+1))
numTree' (ABranch l d r) n = do 
					numTree' l
					numTree' r
					return (ALeaf (d,n))
-}

{-
enumS :: ATree Double -> State Int (ATree (Int,Double))
enumS (ALeaf d) = do 
					
enumS (ABranch l d r) = do
-}