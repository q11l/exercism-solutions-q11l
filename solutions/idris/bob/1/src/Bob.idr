module Bob
import Data.String

notLowerCase : Int -> Bool
notLowerCase decimal = not (decimal > 96 && decimal < 123)

isUpperCase : Int -> Bool
isUpperCase decimal = (decimal > 64 && decimal < 91)

returnYell : Char -> String
returnYell x = case x of
                   '?' => "Calm down, I know what I'm doing!"
                   _   => "Whoa, chill out!"

returnAnswer : Char -> String
returnAnswer x = case x of
                 '?' => "Sure."
                 _   => "Whatever."

bobResponse : Bool -> Maybe Bool -> List Char -> String
bobResponse _ _ Nil = "Fine. Be that way!"
bobResponse True (Just True) (x::Nil) = returnYell x
bobResponse False (Just onlyUpperCase) (x::Nil) = if onlyUpperCase && x == '!'
                                         then returnYell x
                                         else returnAnswer x
bobResponse _ _ (x::Nil) = returnAnswer x
bobResponse _ onlyUpperCase (x::xs) = let decimal = ord x
                                          someCase = case onlyUpperCase of
                                                     Nothing => True
                                                     Just a  => a
                                          updateCase = if someCase
                                                       then notLowerCase decimal
                                                       else False
                                          maybeUpperCase = isUpperCase decimal
                                      in bobResponse maybeUpperCase (Just updateCase) xs

export
response : String -> String
response x = bobResponse False Nothing $ unpack $ trim x