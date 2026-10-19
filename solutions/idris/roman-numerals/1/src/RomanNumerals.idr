module RomanNumerals

import Data.Nat

scale1 : Nat -> String
scale1 1000 = "M"
scale1 100  = "C"
scale1 10   = "X"
scale1 1    = "I"
scale1 _    = ""

scale5 : Nat -> String
scale5 500 = "D"
scale5 50  = "L"
scale5 5   = "V"
scale5 _   = ""

fromArabic : Nat -> Nat -> String
fromArabic unit x with (mod (div x unit) 10)
    fromArabic unit x | digit =
     if x < unit then "" else
      if digit == 0 then fromArabic (unit * 10) x else
        if digit < 5
        then buildDigit 1 4 scale1 scale5
        else buildDigit 5 9 scale5 scale1
      where
        buildRem : Nat -> String
        buildRem 0 = ""
        buildRem rem = scale1 unit ++ buildRem (minus rem unit)
        
        buildDigit : Nat -> Nat -> 
                    (f1, f2 : Nat -> String) 
                    -> String
        buildDigit scale maximum f1 f2 =
            fromArabic (unit * 10) x ++
                if digit == maximum
                then scale1 unit ++ f2 (unit * (maximum + 1))
                else (f1 (scale * unit)) ++ buildRem (minus digit scale * unit)

export
roman : Int -> String
roman = fromArabic 1 . cast