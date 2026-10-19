module LargestSeriesProduct

public export
data Error = StringContainsNonDigit | SpanExceedsStringLength

compareSpansProducts : Nat -> Nat -> List Char -> Either Error Int 
compareSpansProducts Z _ _   = Right 0
compareSpansProducts _ _ Nil = Right 0
compareSpansProducts (S remSpans) span array@(x::xs) = 
    let nextGreater = compareSpansProducts remSpans span xs in
    case buildSpanProduct span array of
    Nothing => Left StringContainsNonDigit
    Just  s => (max s) <$> nextGreater 
where
    buildSpanProduct : (span : Nat) -> List Char -> Maybe Int
    buildSpanProduct (S k) (x::xs) = 
        if isDigit x 
        then ((ord x - 48) *) <$> buildSpanProduct k xs
        else Nothing
    buildSpanProduct Z _ = Just 1
    buildSpanProduct (S _) Nil = Nothing

export
largestProduct : String -> Nat -> Either Error Int
largestProduct digits span = 
    let charDigits = unpack digits
        lenDigits  = length charDigits in
        if span == 0 
        then Right 1
        else if lenDigits < span
             then Left SpanExceedsStringLength
             else if lenDigits == 0
                  then Left StringContainsNonDigit
                  else let totalSpans = minus lenDigits span + 1 in
                  compareSpansProducts totalSpans span charDigits 