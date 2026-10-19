module SumOfMultiples
import Data.Vect as V
import Data.Fin as F

getMultiples : (n : Nat) -> Nat -> Vect n Bool -> Nat -> (Int, Vect n Bool)
getMultiples _ 0 box 0 = (0, box)
getMultiples n factor box acc = 
    let num = acc + factor
    in case F.natToFin num n of
       Nothing  => (0, box)
       (Just a) => let newBox = V.replaceAt a True box
                       (nextNum, nextBox) = getMultiples n factor newBox num
                       newNum : Int = if V.index a box then 0 else cast num
                   in  (newNum + nextNum, nextBox)

build : (n : Nat) -> List Int -> Vect n Bool -> Int
build _ Nil     box = 0
build n (x::xs) box = let (number, newBox) = getMultiples n (cast x) box 0
                      in  number + build n xs newBox

export
sum : List Int -> Int -> Int
sum factor limit = build (cast limit) factor $ V.replicate (cast limit) False