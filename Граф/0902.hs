import System.IO
data Person = Person {name:: String, age :: Integer, weight :: Integer} deriving (Eq,Show,Read)
p1 = Person "Pasha" 24 85

{-
main = do
	writeFile "firstOutput.txt" (show p1)
-}

main = do
	secondPerson <- readFile "1.txt" 
	writeFile "secondOutput.txt" ("Person {name = " ++ name(secondPerson)++ ", age = " ++ age(secondPerson) ++ ", weight = " ++ weight(secondPerson))
