module KindergartenGarden

import Data.Vect
import Data.Fin

public export
data Plant = Grass | Clover | Radishes | Violets

data States = Select Plant | Continue | Error

students : Vect 12 String
students = 
    [
     "Alice",
     "Bob",
     "Charlie",
     "David",
     "Eve",
     "Fred", 
     "Ginny", 
     "Harriet", 
     "Ileana", 
     "Joseph", 
     "Kincaid", 
     "Larry"
    ]

buildPlant : Char -> States
buildPlant 'G' = Select Grass
buildPlant 'C' = Select Clover
buildPlant 'R' = Select Radishes 
buildPlant 'V' = Select Violets
buildPlant _   = Error

get : Nat -> Nat -> Nat -> Char -> States
get len idx search x = 
    if Vect.hasAny [idx] [search, (search + 1), (search + 1 + len * 2), (search + 2 + len * 2)]
    then buildPlant x
    else Continue

go : Nat -> Nat -> Nat -> List Char -> Maybe (List Plant)
go _ _ _ Nil = Just []
go len idx search (x::xs) =
    case get len idx search x of
    Error    => Nothing
    Continue => go len (S idx) search xs
    Select a => 
        case go len (S idx) search xs of 
        Just  b => Just $ a :: b
        Nothing => Nothing 
go _ _ _ _ = Nothing

export
plants : String -> String -> Maybe (Vect 4 Plant)
plants diagram student = 
    case Vect.elemIndex student students of
    Nothing => Nothing
    Just  a => 
        let search = 2 * Fin.finToNat a 
            len = div (length diagram) 4 in
        case go len 0 search (unpack diagram) of
        Nothing => Nothing
        Just  b => Vect.toVect 4 b