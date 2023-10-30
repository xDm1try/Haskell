import Data.Time
data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =[ DbDate (UTCTime (fromGregorian 1911 5 1)(secondsToDiffTime 34123)), DbNumber 9001, DbString "Hello, world!", DbDate (UTCTime(fromGregorian 1921 5 1)(secondsToDiffTime 34123)), DbNumber 102563]


filterDbDate (DbString x:xs) = filterDbDate xs
filterDbDate (DbNumber x:xs) = filterDbDate xs
filterDbDate (DbDate x:xs) = x: filterDbDate xs
filterDbDate [] = []

filterDbNumber::[DatabaseItem]->[Integer]
filterDbNumber (DbString x:xs) = filterDbNumber xs
filterDbNumber (DbDate x:xs) = filterDbNumber xs
filterDbNumber (DbNumber x:xs) = x: filterDbNumber xs
filterDbNumber [] = []

mostRecent x = maximum (mostRecent1 x)
mostRecent1 (DbString x:xs) = mostRecent1 xs
mostRecent1 (DbNumber x:xs) = mostRecent1 xs
mostRecent1 (DbDate x:xs) = x: mostRecent1 xs
mostRecent1 [] = []



sumDb y = sumDb2 0 y
sumDb2 x y = foldl1 (+) x
	where x = sumDb1 y
sumDb1 (DbString x:xs) = sumDb1 xs
sumDb1 (DbDate x:xs) = sumDb1 xs
sumDb1 (DbNumber x:xs) = x: sumDb1 xs
sumDb1 [] = []


avgDb x = avgDb1 0 x
avgDb1 z x = calcMeanList z
		where z = filterDbNumber x
calcMeanList (x:xs) = doCalcMeanList (x:xs) 0 0

doCalcMeanList (x:xs) s l =  doCalcMeanList xs (s+x) (l+1)
doCalcMeanList [] s l = div s l