module PerfectNumbers

public export
data Category = Perfect | Abundant | Deficient

check : Int -> Int -> Int
check _ 0 = 0
check 1 1 = 0
check number less = 
    let someNum = if mod number less /= 0
                  then 0
                  else let divLess = div number less
                       in if less == divLess || divLess == number
                          then less
                          else less + divLess
    in someNum + check number (less - 1)

export
classify : Int -> Maybe Category
classify number = 
    if number < 1 then Nothing
    else let num : Int = cast number
             sqrtNum = cast $ sqrt $ cast num
             sum = check num sqrtNum
         in if sum == num then Just Perfect
            else if sum < num then Just Deficient
                 else Just Abundant