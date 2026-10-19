module DifferenceOfSquares

square : Integer -> Integer
square n = n * n

export
squareOfSum : Integer -> Integer
squareOfSum number = square $ totalSum number 0 where
                     totalSum : Integer -> Integer -> Integer
                     totalSum 0 acc = acc
                     totalSum n acc = totalSum (n - 1) (acc + n)
                     
export
sumOfSquares : Integer -> Integer
sumOfSquares 0 = 0
sumOfSquares number = (square number) + (sumOfSquares $ number -1)

export
differenceOfSquares : Integer -> Integer
differenceOfSquares number = (squareOfSum number) - (sumOfSquares number)
