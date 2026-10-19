module IsbnVerifier
import Data.Nat

tryHyphen : Char -> Maybe Nat
tryHyphen x = if x == '-' || x == '_' then Just 0 else Nothing 
    
tryDecimal : Char -> Maybe Nat 
tryDecimal x = if x <= '9' && x >= '0' then Just $ cast $ ord x - 48 else Nothing
         
tryTen : Char -> Maybe Nat
tryTen x = if x == 'X' then Just 10 else Nothing
    
verify : Nat -> List Char -> Nat -> Bool
verify (S Z) (x::Nil) acc = 
  case (tryDecimal x, tryTen x) of
  (Nothing, Just a) => 0 == mod (acc + a) 11
  (Just a, Nothing) => 0 == mod (acc + a) 11
  _                 => False
verify idx@(S k) (x::xs) acc =
  case (tryDecimal x, tryHyphen x) of
  (Nothing, Just a) => verify idx xs acc
  (Just a, Nothing) => verify k xs $ a * idx + acc
  _                 => False
verify _ _ _ = False

export
isValid : String -> Bool
isValid s = verify 10 (unpack s) 0