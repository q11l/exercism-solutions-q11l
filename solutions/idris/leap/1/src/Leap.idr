module Leap

export
isLeap : Int -> Bool
isLeap year = case (mod year 4, mod year 400, mod year 100) of
               (0, 0, 0)  => True
               (0, _, 0)  => False
               (0, _, _) => True
               _ => False
