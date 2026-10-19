module ZebraPuzzle

import Data.Vect
import Data.Fin
import Data.Zippable
import Data.List
import Data.Either

zipApp : Applicative f => (a -> a -> f a) -> Vect n a -> Vect n a -> f (Vect n a)
zipApp _ Nil Nil = pure Nil
zipApp fun (x :: xs) (y :: ys ) = [| fun x y :: zipApp fun xs ys |]

firstJust : List (Maybe a) -> Maybe a
firstJust l = join $ find isJust l

public export
data Nationality = Englishman
                 | Japanese
                 | Norwegian
                 | Spaniard
                 | Ukrainian

public export
record Solution where
  constructor MkSolution
  drinksWater : Nationality
  ownsZebra : Nationality

Duo : Type
Duo = ((Nat, Fin 5), (Nat, Fin 5))

Matrix : Type    
Matrix = Vect 5 (Vect 5 Nat)

empty : Vect 5 Nat
empty = [0, 0, 0, 0, 0]

ifZero : Nat -> Nat -> Either (Vect 1 Nat) (Vect 1 Nat)
ifZero value Z = Right [value]
ifZero value k = Left [k]
 
ifZero2 : Nat -> Nat ->
          Maybe (Either (Vect n Nat) (Vect n Nat)) ->
          Maybe (Either (Vect (S n) Nat) (Vect (S n) Nat))
ifZero2 _ _ Nothing = Nothing
ifZero2 value x (Just ns)
  = case (x, ns) of
         (Z, Left  ls) => Just $ Right $ value :: ls
         (Z, Right rs) => Nothing
         (k, Right rs) => Just $ Right $ k :: rs
         (k, Left  ls) => Just $ Left  $ k :: ls
 
singleZero : Vect 5 Nat -> 
             Vect (S n) (Vect 5 Nat) ->
             Vect 5 (Maybe (Either (Vect (S n) Nat) (Vect (S n) Nat)))
singleZero values (column :: Nil) = zipWith (\x, y => Just $ ifZero x y) values column
singleZero values (c1 :: c2 :: columns)
  = zipWith3 ifZero2 values c1 $ singleZero values (c2 :: columns)
  
tryPair : Vect (S n) (Vect 5 Nat) ->
          Duo ->
          Maybe (Vect (S n) (Vect 5 Nat))
tryPair (_ :: Nil) _ = Nothing
tryPair (o1 :: o2 :: ordered) pair@((left, lIndex), (right, rIndex))
    = let idx1 = index lIndex o1
          idx2 = index rIndex o2
       in firstJust [ toMaybe (idx1 == left && idx2 == right) $  o1 :: o2 :: ordered
                    , toMaybe (idx1 == left && idx2 == 0) $ o1 :: replaceAt rIndex right o2 :: ordered
                    , toMaybe (idx2 == right && idx1 == 0) $ replaceAt lIndex left o1 :: o2 :: ordered
                    , [| (Just o1 :: tryPair (o2 :: ordered) pair) |]
                    ]

loopPairs : List (Duo, Maybe Duo) ->
            List (Duo, Maybe Duo) ->
            Matrix ->
            List (List (Duo, Maybe Duo), Matrix)
loopPairs _ Nil positions = []
loopPairs sx (x@(left, right) :: xs) ordered
  = let neighbors = (sx ++ xs)
        l = case tryPair ordered left of
                 Nothing => loopPairs (x :: sx) xs ordered
                 Just mL => (neighbors, mL) :: loopPairs sx xs mL
        r = do justRight <- right
               mR <- tryPair ordered justRight
               Just ((neighbors, mR) :: loopPairs sx xs mR)
     in l ++ (fromMaybe [] r)
    
tryFit : Nat -> Nat -> Maybe Nat
tryFit Z y = Just y
tryFit x Z = Just x
tryFit x y = Nothing

prf2 : (n : Nat) -> (len : Nat) ->
       n + (S len) === 5 -> 
       (S n) + len === 5
prf2 n len p
  = rewrite plusCommutative (S n) len in      --> (S n) + len
    rewrite sym $ plusSuccRightSucc len n in  --> len + (S n)
    rewrite cong S $ plusCommutative len n in --> S (len + n)
    rewrite plusSuccRightSucc n len in        --> S (n + len)
    p                                         --> n + (S len)

tryUnify : {n : Nat} -> {m : Nat} ->
             {auto prf : n + m === 5} ->
             Vect n (Vect 5 Nat) ->
             Vect m (Vect 5 Nat) ->
             Vect 5 Nat -> 
             List Matrix
tryUnify {m=Z} _ Nil _ = []
tryUnify {m=S len} sy (y1 :: ys) matrix
    = let next = tryUnify {prf = prf2 n len prf} (y1 :: sy) ys matrix in
      case zipApp tryFit y1 matrix of
           Nothing => next
           Just  a => replace {p = \k => Vect k (Vect 5 Nat)} prf (reverse sy ++ (a :: ys)) :: next

toValues : Vect 5 Nat -> 
           (Duo, Maybe Duo) -> 
           Vect 5 Nat
toValues x (((left, indexL), (right, indexR)), xs)
  = replaceAt indexL left $ replaceAt indexR right x

from : Maybe (Either a a) -> a -> a
from Nothing y = y
from (Just x) _ = fromEither x

LoopFirm : Type
LoopFirm =  List (Duo, Maybe Duo) -> 
            List (Vect 5 Nat) ->
            Matrix -> 
            Maybe Matrix

solveGroups : LoopFirm ->
              List (Vect 5 Nat) ->
              (List (Duo, Maybe Duo), Matrix) ->
              Maybe Matrix
solveGroups loop Nil (duos, matrix) = loop duos [] matrix
solveGroups loop (x :: xs) (duos, m)
    = firstJust $ loop duos xs <$> tryUnify [] m x

loop : LoopFirm
loop Nil Nil matrix = Just matrix
loop Nil groups matrix = solveGroups loop groups ([], matrix)
loop duos groups ordered
  = case loopPairs [] duos ordered of
         Nil => let s = singleZero (foldl toValues empty duos) ordered
                    u = transpose $ zipWith from s (transpose ordered)
                 in case groups of
                         Nil => if u == ordered 
                                   then Nothing 
                                   else loop duos [] u   
                         _ => solveGroups loop groups (duos, u)
         listDuos => join $ List.find isJust ((\(x,y) => loop x groups y) <$> listDuos)

findZero : Fin 5 -> 
           Vect n (Vect 5 Nat) -> 
           Maybe (Vect 5 Nat)
findZero i Nil = Nothing
findZero i (x :: xs) =
  if index i x == 0 
     then Just x 
     else findZero i xs

toNationality : Nat -> Maybe Nationality
toNationality 2  = Just Englishman
toNationality 15 = Just Japanese
toNationality 22 = Just Norwegian
toNationality 3  = Just Spaniard
toNationality 7  = Just Ukrainian
toNationality _  = Nothing

getSolution : Fin 5 -> Maybe Matrix -> Nationality
getSolution i m
  = fromMaybe Englishman $
      do m1 <- m
         fz <- findZero i m1
         toNationality $ index 1 fz
         
-- Color | Nationality | Beverage | Pet | Hobby

groups : List (Vect 5 Nat)
groups =
  [ [1, 2, 0, 0, 0]   -- Red = 1       | Englishman = 2
  , [0, 3, 0, 4, 0]   -- Spaniard = 3  | Dog = 4
  , [5, 0, 6, 0, 0]   -- Green = 5     | Coffee = 6
  , [0, 7, 8, 0, 0]   -- Ukranian = 7  | Tea = 8
  , [0, 0, 0, 9, 10]  -- Snail = 9     | Dancing = 10
  , [11, 0, 0, 0, 12] -- Yellow = 11   | Painter = 12
  , [0, 0, 13, 0, 14] -- Juice = 13    | Football = 14
  , [0, 15, 0, 0, 16] -- Japanese = 15 | Chess = 16
  ]

duos : List (Duo, Maybe Duo)
duos
  = [ (((19, 0), (5, 0)), Nothing) -- Ivory = 19
    , (((20, 4), (21, 3)), Just ((21, 3), (20, 4))) -- Reading = 20   | Fox = 21
    , (((22, 1), (23, 0)), Just ((23, 0), (22, 1))) -- Norwegian = 22 | blue = 23
    , (((12, 4), (18, 3)), Just ((18, 3), (12, 4))) -- Painter = 12   | Horse = 18
    ]

ordered : Matrix
ordered 
    = [ [0, 22, 0, 0, 0]
      , empty
      , [0, 0, 17, 0, 0]
      , empty
      , empty
      ] -- Milk = 17

export
solution : Solution
solution =
  let s = loop duos groups ordered in
  MkSolution (getSolution 2 s) (getSolution 3 s)