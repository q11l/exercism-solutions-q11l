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

charToPlant : Char -> States
charToPlant 'G' = Select Grass
charToPlant 'C' = Select Clover
charToPlant 'R' = Select Radishes 
charToPlant 'V' = Select Violets
charToPlant _   = Error

isStudentPlant : Nat -> Nat -> Nat -> Char -> States
isStudentPlant len idx search x = 
    if Vect.hasAny [idx] [search, (search + 1), (search + 1 + len * 2), (search + 2 + len * 2)]
    then charToPlant x
    else Continue

loopDiagram : Nat -> Nat -> Nat -> List Char -> Maybe (List Plant)
loopDiagram _ _ _ Nil = Just []
loopDiagram len idx search (x::xs) =
    case isStudentPlant len idx search x of
    Error    => Nothing
    Continue => loopDiagram len (S idx) search xs
    Select a => 
        case loopDiagram len (S idx) search xs of 
        Just  b => Just $ a :: b
        Nothing => Nothing 
loopDiagram _ _ _ _ = Nothing

export
plants : String -> String -> Maybe (Vect 4 Plant)
plants diagram student = 
    let len = div (length diagram) 4 in
    do
    idx <- Vect.elemIndex student students 
    plantsList <- loopDiagram len 0 (2 * Fin.finToNat idx) (unpack diagram)
    Vect.toVect 4 plantsList