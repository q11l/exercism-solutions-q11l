module Sieve

import Data.Vect
import Data.Fin

export
primes : Nat -> List Nat
primes limit = reverse $ findPrimes [limit..0] 
  where
    
    isPrime : Nat -> List Nat -> Bool
    isPrime i Nil = 1 < i
    isPrime i (x::xs) =
        if 1 < x && mod i x == 0
        then False
        else isPrime i xs
    
    findPrimes : List Nat -> List Nat
    findPrimes Nil = []
    findPrimes (x::xs) = 
            if isPrime x xs
            then x :: findPrimes xs
            else findPrimes xs