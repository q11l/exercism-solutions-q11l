module Raindrops

hear : Int -> Int -> String -> String
hear number d sound = if mod number d == 0 then sound else ""                          

hearAll : Int -> String
hearAll number = (hear number 3 "Pling") ++ (hear number 5 "Plang") ++ (hear number 7 "Plong")

export
convert : Int -> String
convert number = let sound = hearAll number 
                 in if length (unpack sound) == 0 then show number else sound