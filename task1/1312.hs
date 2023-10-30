import TEXT.REGEX.PCRE

isBin:: String -> Bool
isBin str = str =~ "^[01]+$"

isPhone:: String -> Bool
isPhone str = str =~ "^(\\+7)(-)([0-9]){3}(-)([0-9]){3}(-)([0-9]){4}"

xor x y = if (x == y) then False else True
newtype XOR = XOR {getXOR :: Bool} deriving (Eq, Show)

instance Semigroup XOR where 
 (XOR x) <> (XOR y) = XOR (xor x y)
 
instance Monoid XOR where 
 mempty = XOR False 
 
 -- (XOR False) <> (XOR True) ++++++++++++++++
 
 newtype Maybe' a = Maybe' {getMaybe :: Maybe a} deriving (Eq, Show)
  (Maybe' (Just a)) <> (Maybe' (Just b)) = Maybe' (Just (a<>b))
   x <> (Maybe' Nothing) = x
   (Maybe' Monoid) <> y = y
instance Monoid a => Monoid (Maybe' a) where 
-- ??? mempty = Maybe' (Just mempty)

-- Maybe' (Just [1,2,3]) <> Maybe' (Just [4,5,6]) +++++++++++++ 