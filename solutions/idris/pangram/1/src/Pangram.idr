module Pangram
import Data.Vect as V

export
isPangram : String -> Bool
isPangram = pan 26 (buildAlphabet 26) . unpack where
            buildAlphabet : (n : Nat) -> Vect n Int
            buildAlphabet Z = []
            buildAlphabet (S k) = [cast $ 97 + k] ++ buildAlphabet k
            
            pan : (n : Nat) -> Vect n Int -> List Char -> Bool  
            pan _ alphabet (x::xs) with (V.delete (ord $ toLower x) alphabet)
                                 _ | (len ** vec) = pan len vec xs
            pan _ Nil _ = True
            pan _ _ _ = False