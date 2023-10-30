{-# UnicodeSyntax #-}

\section{Прямые и плоскости в 3-мерном пространстве}

\begin{code}

module LinesPlanes where

import AnGeo
import Lines

-- import Data.Semigroup
-- import Data.Monoid

\end{code}


Зададим тип данных "плоскость", задаваемый скалярным произведением:
$$
(\vec{r}_0 - \vec{r}) \cdot \vec{n} = 0,
$$
т.е. описываем точки плоскости в которые приходит радиус-вектор $\vec{r}$ с помощью радиус-вектора начальной точки $\vec{r}_0$ и нормали $\vec{n}$.

\begin{code}
data Plane = Pl {mo, normal :: Vec} deriving (Read)
\end{code}

Зададим тип данных "каноническое уравнение плоскости" в соответствии с каноническим уравнением плоскости:
$$
Ax + By + Cz + D = 0.
$$

\begin{code}
data CPlane = CPl {aa,bb,cc,dd :: Double} deriving (Read)
\end{code}

Зададим функцию нахождения нормали для плоскости, заданной в канонической форме

\begin{code}
normalForCPlane :: CPlane -> Vec
normalForCPlane (CPl a b c _) = Vc a b c
-- normalForCPlane (CPl a b c d) = Vc a b c
\end{code}

Зададим функции-конструкторы плоскости:

\begin{code}
planeFromPointAndVec :: Point -> Vec -> Plane
planeFromPointAndVec p u = Pl u (((fromPoint p) `mns` u) `vprod` u)
\end{code}

(неплохо бы обдумать вырожденные случаи)

\begin{code}
planeFrom3Points :: Point -> Point -> Point -> Plane
planeFrom3Points p1 p2 p3 = Pl ((fromPoint p1) `mns` (fromPoint p2)) ((fromPoint p1) `mns` (fromPoint p2) `vprod` ((fromPoint p3) `mns` (fromPoint p2)))
\end{code}

\begin{code}
planeFrom2Lines :: Line -> Line -> Plane
planeFrom2Lines l1 l2 = Pl (dir l1) ((dir l1) `vprod` (dir l2))
\end{code}

Преобразование типов плоскостей:

\begin{code}
planeToCPlane :: Plane -> CPlane
planeToCPlane (Pl mo (Vc a b c)) = (CPl a b c (-((mo) `sprod` (Vc a b c))))
\end{code}

\begin{code}
cplaneToPlane :: CPlane -> Plane
cplaneToPlane (CPl a b c d) = (Pl ((fromPoint (Pt a 0 0)) `mns` (fromPoint (Pt 0 b 0))) (fromPoint (Pt a b c)))
\end{code}

Красивое отображение канонической плоскости в виде уравнения:

\begin{code}
instance Show CPlane where
  show cplane = show (aa cplane) ++ "x" ++ " + " ++ show (bb cplane) ++ "y" ++ " + " ++ show (cc cplane) ++ "z" ++ " + " ++ show (dd cplane)
\end{code}

Проверка принадлежености точки плоскости (в обеих формах)

\begin{code}
pointOnPlane :: Point -> Plane -> Bool
pointOnPlane p pln = pointOnCPlane p (planeToCPlane pln)

pointOnCPlane :: Point -> CPlane -> Bool
pointOnCPlane p cpln = (aa cpln)*(px p) + (bb cpln)*(py p) + (cc cpln)*(pz p) + (dd cpln) == 0
\end{code}

Проверка принадлежености прямой плоскости

\begin{code}
lineOnPlane :: Line -> Plane -> Bool
lineOnPlane ln pln = (pointOnPlane (beg (toOrSeg (ro ln))) pln) && (pointOnPlane (end (toOrSeg (ro ln))) pln) && (pointOnPlane (beg (toOrSeg (dir ln))) pln) && (pointOnPlane (end (toOrSeg (dir ln))) pln)

lineOnCPlane :: Line -> CPlane -> Bool
lineOnCPlane ln pln = (pointOnCPlane (beg (toOrSeg (ro ln))) pln) && (pointOnCPlane (end (toOrSeg (ro ln))) pln) && (pointOnCPlane (beg (toOrSeg (dir ln))) pln) && (pointOnCPlane (end (toOrSeg (dir ln))) pln)
\end{code}

Проверка совпадения двух плоскостей

\begin{code}
 --instance Eq Plane where


instance Eq CPlane where
 pl1 == pl2 = (aa pl1)/(aa pl2) == (bb pl1)/(bb pl2)&&(cc pl1)/(cc pl2)==(dd pl1)/(dd pl2)&&(bb pl1)/(bb pl2)==(cc pl1)/(cc pl2)
\end{code}

Проверка параллельности двух плоскостей

\begin{code}
planeParall :: Plane -> Plane -> Bool
planeParall pl1 pl2 = coll (normal pl1) (normal pl2)

cplaneCParall :: CPlane -> CPlane -> Bool
cplaneCParall pl1 pl2 = (aa pl1)/(aa pl2) == (bb pl1)/(bb pl2)&&(bb pl1)/(bb pl2)==(cc pl1)/(cc pl2)
\end{code}

Проверка перпедикулярности двух плоскостей

\begin{code}
planePerp :: Plane -> Plane -> Bool
planePerp p1 p2 = (normal p1) `perp` (normal p2)

cplanePerp :: CPlane -> CPlane -> Bool
cplanePerp (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2) = (Vc a1 b1 c1) `perp` (Vc a2 b2 c2)
\end{code}

Проверка параллельности прямой и плоскости

\begin{code}
lineAndPlaneParall :: Line -> Plane -> Bool
lineAndPlaneParall line plane = (dir line) ┴ (normal plane)

lineAndCPlaneParall :: Line -> CPlane -> Bool
lineAndCPlaneParall ln pln = (dir ln) ┴ (normalForCPlane pln)
\end{code}

Проверка перпедикулярности прямой и плоскости

\begin{code}
lineAndPlanePerp :: Line -> Plane -> Bool
lineAndPlanePerp ln pln = (dir ln) `coll` (normal pln)

lineAndCPlanePerp :: Line -> CPlane -> Bool
lineAndCPlanePerp ln pln = (dir ln) `coll` (normalForCPlane pln)
\end{code}

Нахождение угла между плоскостями (в градусах бы)...

\begin{code}
planeAngle :: Plane -> Plane  -> Double
planeAngle pl1 pl2 = acos ( ((normal pl1) `sprod` (normal pl2)) / ((norma (normal pl1))*(norma (normal pl2))) )*(57.295780)

cplaneAngle :: CPlane -> CPlane  -> Double
cplaneAngle pl1 pl2 = acos  ((((normalForCPlane pl1) `sprod` (normalForCPlane pl2))) /( (norma(normalForCPlane pl1)*(norma(normalForCPlane pl2)) )))*(57.295780)
\end{code}

Нахождение угла между прямой и плоскостью (в градусах бы)...

\begin{code}
lineAndPlaneAngle :: Line -> Plane  -> Double
lineAndPlaneAngle ln pln = asin ( ((dir ln)`sprod` (normal pln)) / ((norma (dir ln)) * (norma (normal pln))) )*(57.295780)

lineAndCPlaneAngle :: Line -> CPlane  -> Double
lineAndCPlaneAngle ln pln = asin ( ((dir ln)`sprod` (normalForCPlane pln)) / ((norma (dir ln)) * (norma (normalForCPlane pln))) )*(57.295780)
\end{code}

Нахождение расстояния между точкой и плоскостью

\begin{code}
pointToPLaneDistance :: Point -> Plane -> Double
pointToPLaneDistance (Pt x y z) pln = pointToCPLaneDistance (Pt x y z) (planeToCPlane  pln)

pointToCPLaneDistance :: Point -> CPlane -> Double
pointToCPLaneDistance (Pt x y z) (CPl a b c d) = abs(a *x + b*y + c*z + d)/(sqrt (a*a + b*b + c*c))
\end{code}

Нахождение линии пересечения двух плоскостей, заданных уравнением...

begin{code}
lineIntersectionOf2Planes :: Plane -> Plane -> Line
lineIntersectionOf2Planes pl1 pl2 = 
end{code}

