module GameOfLife
import Data.Vect

updateState : Nat -> Nat -> Fin 2
updateState 1 2 = 1
updateState 1 3 = 1
updateState 0 3 = 1
updateState _ _ = 0

sumRow : Num a => Vect (S (S n)) a -> Vect (S n) a
sumRow (x :: xs) with (xs)
  _ | (y :: Nil) = [x + y]
  _ | (y :: z :: _) = x + y + z :: sumRow xs

updateRow : Vect (S n) Nat
         -> Vect (S (S n)) Nat
         -> Vect (S n) Nat
         -> Vect (S n) (Fin 2)
updateRow (sum1 :: sums1) (s1 :: states) (sum2 :: sums2) with (states)
  _ | (s2 :: Nil) = [updateState s2 $ sum1 + s1 + sum2]
  _ | (s2 :: s3 :: _) =
    updateState s2 (sum1 + s1 + s3 + sum2) :: updateRow sums1 states sums2

export
tick : (rows : Nat) -> (columns : Nat) -> Vect rows (Vect columns (Fin 2)) -> Vect rows (Vect columns (Fin 2))
tick Z _ _ = []
tick r Z _ = replicate r []
tick (S Z) c _ = [replicate c FZ]
tick r (S Z) _ = replicate r [FZ]
tick row@(S r) col@(S c) matrix@(m1 :: _) with (replicate col Z)
  _ | emptyVect =
    let nat3 = finToNat <$> m1 in
    loopRows nat3 emptyVect (sumRow $ Z :: nat3) matrix
    where

    loopRows : Vect (S c) Nat
            -> Vect (S c) Nat
            -> Vect (S c) Nat
            -> Vect (S n) (Vect (S c) (Fin 2))
            -> Vect (S n) (Vect (S c) (Fin 2))
    loopRows nat2 sums1 sums2 (r2 :: Nil) =
      [updateRow sums1 (Z :: nat2) emptyVect]
    loopRows nat2 sums1 sums2 (r2 :: r3 :: rows) =
        let nat3 = finToNat <$> r3
            sums3 = sumRow (Z :: nat3)
            updated = updateRow sums1 (Z :: nat2) sums3 in
        updated :: loopRows nat3 sums2 sums3 (r3 :: rows)
