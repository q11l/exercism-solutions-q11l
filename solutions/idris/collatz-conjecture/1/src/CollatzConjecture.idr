module CollatzConjecture

rules : Int -> Int
rules number = if 0 == (mod number 2)
               then div number 2
               else number * 3 + 1

export
steps : Int -> Maybe Int
steps 1      = (Just 0)
steps number = if 0 < number
               then case (steps $ rules number) of
                        Nothing  => Nothing
                        (Just a) => (Just $ 1 + a)
               else Nothing