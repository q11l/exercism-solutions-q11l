module NthPrime

import Data.Nat

export
prime : Nat -> Nat
prime number = loop number 2
  where
    isPrime : Int -> Int -> Int -> Bool
    isPrime i idx limit =
      if limit < idx then True else 
        if 1 < idx && mod i idx == 0
        then False
        else isPrime i (idx + 1) limit
    
    nextPrime : Nat -> Nat
    nextPrime Z = 0
    nextPrime x@(S k) =
        if isPrime (cast x) 2 (cast $ sqrt $ cast x)
        then x 
        else nextPrime (S x)

    loop : Nat -> Nat -> Nat
    loop Z _ = 2
    loop (S Z) lastPrime = lastPrime 
    loop (S k) lastPrime = loop k (nextPrime (S lastPrime))