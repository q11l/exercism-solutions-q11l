module Grains
import Data.Bits as B

export
grains : Integer -> Maybe Integer
grains square = if 64 < square || square < 1 then Nothing 
                else (Just $ powerBase2 square 1) where
                powerBase2 : Integer -> Integer -> Integer
                powerBase2 1 acc = acc
                powerBase2 count acc = powerBase2 (count - 1) (acc * 2) 

--
export
totalGrains : Integer
totalGrains = case (grains 64) of
              Nothing => 0
              (Just a) => (a * 2) - 1