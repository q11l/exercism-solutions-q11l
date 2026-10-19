
module PythagoreanTriplet

import Data.Vect

export
tripletsWithSum : Int -> List (Vect 3 Int)
tripletsWithSum n = findTriplet 1 (cast $ div n 3)
    where
    findTriplet : Int -> Nat -> List (Vect 3 Int)
    findTriplet x Z = []
    findTriplet x (S k) =
        let next =  findTriplet (x + 1) k
            num = n * (n - 2 * x)  
            d = 2 * (n - x) in
        if mod num d /= 0 then next else 
            let y = div num d
                z = n - y - x in
        if x < y && y < z && (x * x) + (y * y) == (z * z)
        then [x, y, z] :: next
        else next