module Darts
import Data.Vect as V

getDistance : (x, y : Double) -> Double
getDistance x y = sqrt (x * x + y * y)

getCircle : Double -> Vect n Int -> Int
getCircle distance Nil = 0
getCircle distance (x::xs) = if distance <= cast x
                             then 0
                             else 1 + getCircle distance xs

getPoint : Vect n Int -> Int -> Int
getPoint Nil _ = 0
getPoint (x::xs) circle = if circle == 0
                          then x
                          else getPoint xs $ circle - 1

radius : Vect 3 Int
radius = [1, 5, 10]

export
score : Double -> Double -> Int
score x y = getPoint (V.reverse radius) $ getCircle (getDistance x y) radius