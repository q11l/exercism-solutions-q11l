module Triangle
import Data.Vect as V

getSides : Vect 3 Integer -> (Integer, Integer, Integer)
getSides sides = (V.index 0 sides, V.index 1 sides, V.index 2 sides)

isValid : Integer -> Integer -> Integer -> Bool
isValid s1 s2 s3 = s1 < s2 + s3 && s2 < s1 + s3 && s3 < s1 + s2

export
isEquilateral : Vect 3 Integer -> Bool
isEquilateral sides = let (s1, s2, s3) = getSides sides
                      in isValid s1 s2 s3 && (s1 == s2 && s2 == s3)

export
isIsosceles : Vect 3 Integer -> Bool
isIsosceles sides = let (s1, s2, s3) = getSides sides
                    in isValid s1 s2 s3 && (s1 == s2 || s2 == s3 || s3 == s1)

export
isScalene : Vect 3 Integer -> Bool
isScalene sides = let (s1, s2, s3) = getSides sides
                  in isValid s1 s2 s3 && (not $ isIsosceles sides)