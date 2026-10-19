module PhoneNumber

data Try = Valid | Invalid | Digit | One

checkNum : Char -> Try
checkNum c = let decimal = ord c
             in if decimal < 58 && 47 < decimal then Digit else Invalid

tryNum : Bool -> Nat -> List Char -> Maybe (List Char)
tryNum b k Nil = if k < 10 then Nothing else Just []
tryNum b k (x::xs) = case try x k of
                     Invalid => Nothing
                     Valid   => tryNum b k xs
                     Digit   => (x ::) <$> tryNum b (S k) xs
                     One     => if b then (tryNum False k xs) else Nothing
                     where
                     try : Char -> Nat -> Try
                     try ' ' _ = Valid
                     try '+' 0 = Valid
                     try '1' 0 = One
                     try '0' 0 = Invalid
                     try '(' 0 = Valid
                     try ')' 3 = Valid
                     try '.' 3 = Valid
                     try '-' 3 = Valid
                     try '0' 3 = Invalid
                     try '1' 3 = Invalid
                     try '.' 6 = Valid
                     try '-' 6 = Valid
                     try _  10 = Invalid
                     try _  _  = checkNum x

export
clean : String -> Maybe String
clean phrase = case tryNum True 0 $ unpack phrase of
               Nothing => Nothing
               (Just number) => (Just $ pack number) 