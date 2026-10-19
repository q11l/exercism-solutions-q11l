module Luhn 
-- Difference from Iteration 1: Removed "length" and "reverse"

import Data.String
import Data.Nat

digit : Char -> Int
digit x = ord x - 48

getDouble : Int -> Int
getDouble x = 
    let double = 2 * x in
        if 9 < double then double - 9 else double 

foldr2Luhn : List Int -> List Int -> Int
foldr2Luhn Nil     Nil     = 0
foldr2Luhn (x::xs) Nil     = x + foldr2Luhn xs []
foldr2Luhn Nil     (y::ys) = getDouble y + foldr2Luhn [] ys
foldr2Luhn (x::xs) (y::ys) = x + getDouble y + foldr2Luhn xs ys

check : Bool -> a -> Maybe a
check b a = if b then Just a else Nothing

verifyChars : Char -> Char -> Maybe Char
verifyChars x y =
    if isDigit x && isDigit y
    then Just y
    else if isDigit x && y == ' '
         then Just '0'
         else Nothing

record Group where
    constructor MkGroup
    isEven : Bool
    left   : List Int
    right  : List Int

divList : Bool -> List Char -> Maybe Group
divList validLen Nil = check validLen $ MkGroup True [] []
divList validLen (x :: Nil) = check validLen $  MkGroup False [digit x] []
divList validLen (' ' :: xs)  = divList validLen xs
divList _ (x1 :: x2 :: xs) = 
    if isDigit x1 && isDigit x2
    then add x2 <$> divList True xs
    else if isDigit x1 && x2 == ' '
         then add '0' <$> divList True ('0'::xs)
         else Nothing
    where
    add : Char -> Group -> Group
    add y group = { left $= (digit x1 ::), right $= (digit y ::) } group

export
isValid : String -> Bool
isValid s = 
    case divList False $ unpack $ String.trim s of
    Nothing => False
    Just group => 
        let (nums, doubles) = if group.isEven 
                              then (group.right, group.left) 
                              else (group.left, group.right) in
        mod (foldr2Luhn nums doubles) 10 == 0