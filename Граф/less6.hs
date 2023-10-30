import Data.Graph
import Data.Array
import Control.Monad.Trans.State
import Data.Sequence hiding (filter)
import Data.Tree
import Prelude hiding (take)
import Data.Foldable


data Color = White | Gray | Black deriving (Eq, Show)

type Coloring = Array Vertex Color 

myG = buildG (0,6) [(0,1),(0,6),(1,2),(2,3),(3,6),(6,4),(6,5),(5,1),(6,2),(4,2)]
gr = buildG (1,6) [(1,2),(1,4),(4,2),(2,5),(5,4),(3,5),(3,6),(6,6)] -- from 1 task
graph = buildG (1,1) [(1,1)]
tst = buildG (1,7) [(2,1),(2,5),(5,3),(5,6),(3,4),(6,4),(6,7),(6,7),(7,6)]

deepFirst :: Graph -> Vertex -> State Coloring Bool
deepFirst g v = do
					colors <- get
					if colors ! v == White
						then do
							put (colors // [(v, Gray)])
							let n = g ! v
							vals <- mapM (deepFirst g) n
							modify (// [(v, Black)])
							return (foldl (||) False vals)
						else return (colors!v == Gray)		

cleanGraph :: Graph -> Int -> Graph                                                    
cleanGraph gr ver = listArray (bounds gr) (map (filter (/=ver)) (elems gr))

queue = empty
--  runState (bfsE gr 3 empty []) (listArray (bounds gr) (cycle [White]))
heaD [] = []
heaD (x:xs) = [x]
fromDigits :: [Int] -> Int
fromDigits xs = aux xs 0
    where aux [] acc = acc
          aux (x:xs) acc  = aux xs ((acc * 10) + x)




bfsE :: Graph -> Vertex -> Seq Vertex -> [Int] -> State Coloring [Int]
bfsE gr v queue list = 	do 
						colors <- get
						let n = gr ! v
						let er = v : list
						if colors ! v == White
						then do
							put (colors // [(v, Gray)])

							bfsE (cleanGraph gr v) (fromDigits (toList (take 1 (deleteAt 0 $ (queue) >< (fromList n))))) (deleteAt 0 $ (queue) >< (fromList n)) list			
							modify (// [(v, Black)])				
							if queue == empty
							then return (er)
							else bfsE (cleanGraph gr v) (fromDigits (toList (take 1 (deleteAt 0 $ (queue) >< (fromList n))))) (deleteAt 0 $ (queue) >< (fromList n)) list
						else if colors ! v == Gray
							 then bfsE (cleanGraph gr v) (fromDigits (toList (take 1 (deleteAt 0 $ (queue) >< (fromList n))))) (deleteAt 0 $ (queue) >< (fromList n)) list	
							 else return (list)
						