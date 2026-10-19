module AllYourBase
public export
data Error = InvalidBase | InvalidDigit

encode : Int -> Int -> Either Error Int
encode x base = 
    if x < 0 || x >= base
    then Left InvalidDigit 
    else Right x

go : List Int -> Int -> Int -> Either Error Int
go Nil _ acc = Right acc
go (x::xs) base acc =
    case encode x base of
    Left  a => Left a
    Right a => go xs base (a + base * acc)

go2 : Int -> Int -> List Int
go2 0 base = []
go2 digit base = 
    let d = mod digit base
     in go2 (div digit base) base ++ [d]

export
rebase : Int -> List Int -> Int -> Either Error (List Int)
rebase inputBase digit outputBase =
     if inputBase < 2 || outputBase < 2
        then Left InvalidBase
        else case go digit inputBase 0 of
             Left  a => Left a
             Right a => if a == 0 
                        then Right [0]
                        else Right $ go2 a outputBase