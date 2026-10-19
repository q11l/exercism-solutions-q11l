module SumOfMultiples
import Data.Vect as V
import Data.Fin as F

getMultiples : (n : Nat) -> Nat -> Vect n Bool -> Nat -> (Int, Vect n Bool)
getMultiples _ 0 seen 0 = (0, seen)
getMultiples n factor seen acc = 
    let num = acc + factor
    in case F.natToFin num n of
       Nothing  => (0, seen)
       (Just a) => let newSeen = V.replaceAt a True seen
                       (nextNum, nextSeen) = getMultiples n factor newSeen num
                       newNum : Int = if V.index a seen then 0 else cast num
                   in  (newNum + nextNum, nextSeen)

build : (n : Nat) -> List Int -> Vect n Bool -> Int
build _ Nil     _    = 0
build n (x::xs) seen = let (number, newSeen) = getMultiples n (cast x) seen 0
                       in  number + build n xs newSeen

export
sum : List Int -> Int -> Int
sum factor limit = build (cast limit) factor $ V.replicate (cast limit) False