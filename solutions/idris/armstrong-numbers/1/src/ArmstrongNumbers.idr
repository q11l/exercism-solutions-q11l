module ArmstrongNumbers

nDigits : Integer -> Integer -> Integer
nDigits 0 _ = 1
nDigits i y = if div i y < 10 then y else nDigits i (y * 10)

digitsFromI : Integer -> Integer -> List Integer
digitsFromI r 1 = [r]
digitsFromI r n = let digit = div r n
                      rest = r - digit * n
                  in digit :: digitsFromI rest (div n 10)

getDigits : Integer -> List Integer
getDigits 0 = [0]
getDigits i = digitsFromI i $ nDigits i 1

pow : Integer -> Integer -> Integer
pow x 0 = 1
pow x n = x * (pow x (n - 1))

export
isArmstrongNumber : Integer -> Bool
isArmstrongNumber number = let digits = getDigits number in
                           let len = cast (length digits) in
                               (number == pows len digits) where
                               pows : Integer -> List Integer -> Integer                
                               pows _ [] = 0
                               pows n (x::xs) = (pow x n) + pows n xs