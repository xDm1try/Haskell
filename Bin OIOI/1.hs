data Bin = End | O Bin | I Bin deriving (Show, Eq)

inc :: Bin -> Bin
inc End = I End
inc (O b) = I b
inc (I b) = O (inc b)

-- O (I (I End)) - представление двоичного числа 110 (6 в десятичной системе)
-- I (I (O End)) - представление двоичного числа 011 (3 в десятичной системе)

prettyPrint :: Bin -> String
prettyPrint End = ""
prettyPrint (I b) = (prettyPrint b) ++ "1"
prettyPrint (O b) = (prettyPrint b) ++ "0"


{-
fromBin :: Bin -> Int
fromBin (O End) = 0
fromBin (I End) = 1
fromBin (O x) = fromBin (x+1) 
fromBin (I x) =(x^2) + fromBin  (x+1) 
-}


toBin 0 = O End
toBin 1 = I End
toBin x | mod x 2 == 0 = toBin0 x
		| mod x 2 == 1 = toBin1 x
toBin0 x = O (toBin (div x 2))  
toBin1 x = I (toBin (div x 2))


pls:: Bin -> Bin -> Bin
pls End  y= End
pls x End = x
pls End x = x
pls (O x) (O y) =O (pls (x) y)
pls (I x) (O y) =I (pls (x) y)
pls (O x) (I y) =I (pls (x) y)
--pls (I x) (I y) =I (pls (x) y)  ??? 

mlt::Bin -> Bin -> Bin
mlt (I End) y =pls (I End) y
mlt (O End) y =pls (O End) y  
mlt (O x) y = O (mlt x y)
mlt (I x) y =pls (y) (mlt x y) 
