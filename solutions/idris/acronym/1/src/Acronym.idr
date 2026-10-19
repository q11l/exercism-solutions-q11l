module Acronym
toUpperCase : Char -> Maybe Char
toUpperCase x = let decimal = ord x
                in if (decimal > 64 && decimal < 91)
                   then (Just x)
                   else if 96 < decimal && decimal < 123
                        then (Just $ chr $ decimal - 32) 
                        else Nothing

getAcronym : Bool -> List Char -> List Char
getAcronym _ Nil     = []
getAcronym b (x::xs) = if x == ' ' || x == '-' || x == '_'
                       then getAcronym True xs
                       else if b
                            then case (toUpperCase x) of
                                 Nothing => getAcronym False xs
                                 (Just c) => c :: getAcronym False xs
                            else getAcronym False xs

export
abbreviate : String -> String
abbreviate x = pack $ getAcronym True $ unpack x