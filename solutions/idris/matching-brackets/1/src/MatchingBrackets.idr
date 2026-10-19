module MatchingBrackets

data Bracket = Parentheses Bool | Curly Bool | Square Bool

count : Char -> Maybe Bracket
count '(' = Just $ Parentheses True
count ')' = Just $ Parentheses False
count '{' = Just $ Curly True
count '}' = Just $ Curly False
count '[' = Just $ Square True
count ']' = Just $ Square False
count  _  = Nothing

cut : Bracket -> Bracket -> Bool
cut (Parentheses True) (Parentheses False) = True
cut (Curly  True)      (Curly  False)      = True
cut (Square True)      (Square False)      = True
cut  _                  _                  = False

verify : List Char -> List Bracket -> Bool
verify Nil Nil = True
verify Nil (y :: ys) = False
verify (x :: xs) Nil = 
    case count x of
    Nothing => verify xs []
    Just  a => verify xs [a]
verify (x :: xs) brackets@(y :: ys) = 
    case count x of
    Nothing => verify xs brackets
    Just  a => 
        if cut y a
        then verify xs ys
        else verify xs (a :: brackets)

export
isPaired : String -> Bool
isPaired s = verify (unpack s) []