;This is, more or less, a port of shape primitives from Paul Hudak's 
;excellent book The Haskell School of Expression.  I'd like to port it 
;eventually...for now it's not doing anything.
(ns spork.geometry.primitive)

;ellipse      :: Point -> Point -> Graphic
;shearEllipse :: Point -> Point -> Point -> Graphic
;line         :: Point -> Point -> Graphic
;polygon      :: [Point] -> Graphic
;polyline     :: [Point] -> Graphic
;
;fillTri x y size w =
;    drawInWindow w
;           (withColor Blue
;            (polygon [(x,y),
;                      (x+size,y),
;                      (x,y-size)]))
;
;
;data Shape = Rectangle Side Side   
; | Ellipse Radius Radius
; | RtTriangle Side Side     
; | Polygon [ Vertex ] 
; deriving Show
;
;type Vertex = (Float,Float) 
;type Side   = Float
;type Radius = Float
;
;
;inchToPixel  :: Float -> Int
;inchToPixel x = round (100*x)
;
;pixelToInch  :: Int -> Float
;pixelToInch n = intToFloat n / 100
;
;intToFloat   :: Int -> Float
;intToFloat  n = fromInteger (toInteger n)
;
;
;trans :: Vertex -> Point
;trans (x,y) = ( xWin2 + inchToPixel x, 
;                yWin2 - inchToPixel y )
;
;transList       :: [Vertex] -> [Point]
;transList []     = []
;transList (p:ps) = trans p : transList ps
;


;;A ray is a vector, originating at a point, that extends infinitely in the 
;;direction defined by another point.  Direction should be a unit-vector.
(defn ->ray [origin dir] {:origin origin :dir (vmath/v-unit dir)}) 
  