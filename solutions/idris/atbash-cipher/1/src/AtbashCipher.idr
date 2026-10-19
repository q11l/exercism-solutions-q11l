module AtbashCipher

left : Int -> Int
left x = if x <= 109 && 97 <= x then 109 + (110 - x) else x

right : Int -> Int
right x = if x >= 110 && x <= 122 then 110 - (x - 109) else left x

clean : Char -> Maybe Char
clean x = 
    case x of
    ' ' => Nothing
    ',' => Nothing
    '.' => Nothing
    _ => Just x

export
encode : String -> String
encode = pack . go 0 . unpack
where
    go : Int -> List Char -> List Char
    go _ Nil = []
    go 5 code@(x::xs::xss) = ' ' :: go 0 code  
    go idx (x::xs) = 
        case (clean . chr . right . ord . toLower) x of
        Nothing => go idx xs
        Just  c => c :: go (idx + 1) xs
    
export
decode : String -> String
decode s = pack $ mapMaybe (clean . chr . right . ord . toLower) (unpack s)