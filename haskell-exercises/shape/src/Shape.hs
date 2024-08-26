module Shape where

data Point = Point { x::Double, y:: Double} deriving (Eq, Show)

data Circle    = Circle    Point Double deriving (Eq, Show)
data Rectangle = Rectangle Point Point deriving (Eq, Show)


-- A point from a tuple Pair
point::(Double, Double) -> Point
point (a, b) = Point {x=a, y=b}

-- The origin
origin::Point
origin = Point {x=0,y=0}

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle::(Double, Double) -> Rectangle
rectangle (b, h) = Rectangle origin (Point {x=b, y=h})

base::Rectangle -> Double
base (Rectangle _ (Point b _) ) = b

height::Rectangle -> Double
height (Rectangle _ (Point _ h) ) = h

-- Circle from radius
circle::Double -> Circle
circle r = Circle origin r

-- Clase Shift

class Shift a where
   shift::a -> (Double, Double) -> a

instance Shift Point where
   shift (Point x y) (a, b) = point(x+a, y+b)

instance Shift Rectangle where
   shift (Rectangle (Point p1x p1y) (Point p2x p2y)) (a, b) = Rectangle (point (p1x+a,  p1y+b)) (point(p2x+a, p2y+b))
   
instance Shift Circle where
   shift (Circle (Point px py) r) (a, b) = Circle (point (px + a, py+b)) r
   
-- Define the Surface class

class Surface a where
    surface:: a -> Double

instance Surface Rectangle where
    surface r = (base r) * (height r)

instance Surface Circle where
    surface (Circle _ r) = pi * r**2
   
