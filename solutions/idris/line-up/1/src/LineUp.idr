module LineUp
import Data.List as L

export
format : String -> Int -> String
format name number = mkMsg name $ show number ++ getEnding number where
                     mkMsg : String -> String -> String
                     mkMsg name number = name ++ ", you are the " ++ number ++ " customer we serve today. Thank you!"

                     getEnding : Int -> String 
                     getEnding number = let lastTwo = mod number 100
                                        in if lastTwo == 11 || lastTwo == 12 || lastTwo == 13 then "th"
                                           else case (mod number 10) of
                                           1 => "st"
                                           2 => "nd"
                                           3 => "rd"
                                           _ => "th"