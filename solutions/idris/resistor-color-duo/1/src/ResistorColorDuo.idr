module ResistorColorDuo

import Data.Vect as V
import Data.Fin as F

valueVec : Vect 10 String
valueVec = 
    [
    "black",
    "brown",
    "red",
    "orange",
    "yellow",
    "green",
    "blue",
    "violet",
    "grey",
    "white"
    ]

export
value : Vect 2 String -> Maybe (Fin 100)
value [x, y] = case ((V.elemIndex x valueVec), (V.elemIndex y valueVec)) of
               ((Just a), (Just b)) => Just (((F.weakenN 90 a) * 10) + (F.weakenN 90 b))
               _ => Nothing