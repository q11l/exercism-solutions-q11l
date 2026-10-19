module Dominoes
import Data.Vect
import Data.List

export
canChain : List (Vect 2 (Fin 7)) -> Bool
canChain Nil = True
canChain ([l, r] :: Nil) = l == r
canChain (l :: r :: Nil) = reverse l == r || l == r
canChain (s1@[l1, r1] :: s2 :: stones) 
  = if l1 /= FZ && r1 /= FZ
    then run r1 [l1, FZ] (s2 :: stones)
    else case s2 of
              [FZ, sR] => run sR s1 stones
              [sL, FZ] => run sL s1 stones
              [_ , _ ] => False
  where
    go : Fin 7 -> Vect 2 (Fin 7) -> 
         List (Vect 2 (Fin 7)) -> Nat -> Bool
    go x firstStone st idx
     = case splitAt idx st of
       (l, Nil) => False
       (l, r :: rs) => case Vect.elemIndex x r of
                            Nothing => False
                            Just  i => let newStone = Vect.replaceAt i FZ r
                                           new = l ++ rs
                                        in canChain (firstStone :: newStone :: new)
                                           

    fff : Fin 7 -> Vect 2 (Fin 7) -> Bool
    fff x [y, z] = (x == y) || (x == z)
    
    run : Fin 7 -> Vect 2 (Fin 7) -> 
          List (Vect 2 (Fin 7)) -> Bool
    run x prevStone st
      = let listIdx = List.findIndices (fff x) st
            fun = go x prevStone st
         in any fun listIdx