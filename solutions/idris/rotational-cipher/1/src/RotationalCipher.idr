module RotationalCipher

move : Int -> List Char -> List Char
move _ Nil = [] 
move rot (x::xs) = 
    let num = ord x    
        fmt = if 64 < num && num < 91
              then modNum num 64 rot 
              else if 96 < num && num < 123
                   then modNum num 96 rot 
                   else num
    in chr fmt :: move rot xs
    where 
    modNum : Int -> Int -> Int -> Int
    modNum num base rot = let fmt = (mod (num - base + rot) 26)
                          in base + if fmt == 0 then 26 else fmt
export
rotate : String -> Int -> String
rotate text shiftKey = pack $ move shiftKey $ unpack text