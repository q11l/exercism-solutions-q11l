module NucleotideCount

public export
record DNA where
    constructor MkDNA
    adenine, cytosine, guanine, thymine : Nat

check : DNA -> Char -> Maybe DNA
check dna c = case c of
             'A' => Just $ {adenine $= S} dna
             'T' => Just $ {thymine $= S} dna
             'C' => Just $ {cytosine $= S} dna
             'G' => Just $ {guanine $= S} dna
             _ => Nothing

checkDna : Maybe DNA -> List Char -> Maybe DNA
checkDna (Just dna) (x::xs) = checkDna (check dna x) xs
checkDna Nothing _ = Nothing
checkDna (Just dna) Nil = (Just dna)

export
nucleotideCounts : String -> Maybe DNA
nucleotideCounts n = checkDna (Just (MkDNA 0 0 0 0)) $ unpack n