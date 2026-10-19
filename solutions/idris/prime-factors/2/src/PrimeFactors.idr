module PrimeFactors

findFactors : Int -> Int -> List Int
findFactors n value = 
    if value == 1 then []
    else if mod value n == 0
         then n :: (findFactors 2 $ div value n)
         else if n == 2 then findFactors (n + 1) value
              else findFactors (n + 2) value
                            
export
factors : Int -> List Int
factors value = findFactors 2 value