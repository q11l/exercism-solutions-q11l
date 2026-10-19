module EliudsEggs
import Data.Bits as B
import Data.Fin as F

export
eggCount : Int -> Int
eggCount number = cast $ count number 0 F.last where
                     getBit : Int -> Fin 64 -> Integer
                     getBit n idx = if n .&. (B.shiftL 1 (idx)) /= 0 then 1 else 0
                     
                     count : Int -> Integer -> Fin 64 -> Integer
                     count n ones FZ = ones + (getBit n FZ)
                     count n ones (FS k) = count n (ones + getBit n (FS k)) (F.weaken k)