module Isogram

export
isIsogram : String -> Bool
isIsogram phrase = (find ' ' $ unpack phrase) where
                   find : Char -> List Char -> Bool
                   find _ Nil = True
                   find c (x::xs) = let (l, r) = (ord c, ord x)
                                        aCase = if r < 91 then r + 32 else r - 32
                                        letter = (l == r) || (l == aCase) 
                                    in if letter && l /= 32 && l /= 45 then False 
                                    else find c xs && find x xs