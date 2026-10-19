module RunLengthEncoding

import Data.String

isNum : Char -> Maybe Int
isNum x = if x >= '0' && x <= '9' then Just $ ord x - 48 else Nothing

some : Int -> String -> String
some count prev = 
    if count == 1 || count == 0
    then prev
    else  "\{show count}\{prev}"

export
encode : String -> String
encode string = loop 0 "" $ unpack string
    where 
    loop : Int -> String -> List Char -> String
    loop count prev Nil = some count prev
    loop count prev (x::xs) = 
        let newString = String.singleton x in
        if newString == prev 
        then loop (count + 1) newString xs
        else some count prev ++ loop 1 newString xs 
--
export
decode : String -> String
decode string = loop 0 $ unpack string
    where
    loop : Int -> List Char -> String
    loop num Nil = ""
    loop num (x::xs) = 
        case isNum x of
        Nothing => singleton x ++ replicate (cast (num - 1)) x ++ loop 0 xs
        Just  a => loop (num * 10 + a) xs