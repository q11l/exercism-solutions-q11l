module ReverseString

export
rev : String -> String
rev str = pack (fun (unpack str)) where
              fun : List Char -> List Char
              fun [] = []
              fun (x::xs) = (fun xs) ++ [x]