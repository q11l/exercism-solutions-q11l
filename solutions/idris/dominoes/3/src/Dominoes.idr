module Dominoes
import Data.Vect
import Data.List

export
canChain : List (Vect 2 (Fin 7)) -> Bool
canChain Nil = True
canChain ([left, right] :: Nil) = left == right
canChain (firstStone :: secondStone :: Nil) 
  = reverse firstStone == secondStone 
    || firstStone == secondStone
canChain (firstStone@[left, right] :: secondStone :: stones) 
  = if left /= FZ && right /= FZ
    then matchSides right [left, FZ] (secondStone :: stones)
    else case secondStone of
              [FZ, rightSide] => matchSides rightSide firstStone stones
              [leftSide, FZ] => matchSides leftSide firstStone stones
              [_, _] => False
  where
    updateStone : Fin 7 -> Vect 2 (Fin 7) -> List (Vect 2 (Fin 7)) -> Nat -> Bool
    updateStone x headStone s idx
     = case splitAt idx s of
       (_, Nil) => False
       (l, r :: rs) => case Vect.elemIndex x r of
                            Nothing => False
                            Just  i => let newStone = Vect.replaceAt i FZ r
                                           newStones = l ++ rs
                                        in canChain (headStone :: newStone :: newStones)                                 
    match : Fin 7 -> Vect 2 (Fin 7) -> Bool
    match x [y, z] = (x == y) || (x == z)
    
    matchSides : Fin 7 -> Vect 2 (Fin 7) -> List (Vect 2 (Fin 7)) -> Bool
    matchSides x headStone s
      = let listIndexes = List.findIndices (match x) s
            fun = updateStone x headStone s
         in any fun listIndexes