import Data.Set
import Data.Maybe
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeSize Empty = 0
treeSize (Node n l r) = treeSize l + treeSize r + 1

myTree = (Node 5 (Node 156 Empty Empty) (Node 1 (Empty) (Node 13 Empty Empty)))

mapTree p Empty = Empty
mapTree p (Node n l r) = (Node (p (n)) (mapTree p l) (mapTree p r)) 


data Step = StepLeft | StepRight    deriving (Show, Eq)



walk [] (Node n l r) = Just n
walk (x:xs) (Node n l r) |x == StepLeft && l /= Empty && r /= Empty = walk xs l
						 |x == StepRight && l /= Empty && r /= Empty = walk xs r
						 |otherwise = Nothing
	


isBinTree p Empty = True
isBinTree p (Node n l r) = (isBinTree p l)&&(isBinTree p r)

--предикат -> (\n l r-> l<=n&&n<r==True) 
--сделал по аналогии с allvalues, но почему-то всегда выдает True,
--когда для myTree соответствует False. 
--Оно должно же все значения учитывать, как в allvalues?
--








