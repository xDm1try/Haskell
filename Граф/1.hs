import Data.Graph
import Data.Tree
import Data.Array
import Data.Sequence
--import Data.Dequeue


graph = buildG (1,6) [(1,2),(1,4),(4,2),(2,5),(5,4),(3,5),(3,6),(6,6)]

data Color = White | Gray | Black deriving (Eq, Show)

type Coloring = Array Vertex Color 

deepFirst :: Graph -> Vertex -> State Coloring Bool