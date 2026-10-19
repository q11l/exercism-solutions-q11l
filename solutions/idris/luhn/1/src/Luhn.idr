module Luhn

import Data.String

digit : Char -> Int
digit x = ord x - 48

getDouble : Char -> Int
getDouble x = let double = 2 * digit x in
                  if 9 < double then double - 9 else double

verify : List Char -> Maybe Int
verify Nil = Just 0
verify (x::Nil) = if isDigit x then Just $ digit x else Nothing
verify (' '::xs)    = verify xs
verify (x1::x2::xs) =
    if isDigit x1 && isDigit x2
    then (digit x1 + getDouble x2 +) <$> verify xs
    else if x2 == ' ' && isDigit x1
         then (digit x1 +) <$> verify ('0'::xs)
         else Nothing

export
isValid : String -> Bool
isValid s = 
    let revStr = reverse $ unpack $ String.trim s in
    if length revStr < 2
    then False
    else case verify revStr of
         Nothing => False
         Just  a => mod a 10 == 0