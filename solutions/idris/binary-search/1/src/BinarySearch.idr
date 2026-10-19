module BinarySearch
import Data.Vect
import Data.Fin
import Decidable.Equality

dropFirst : (n: Nat) -> Vect n a -> (m : Nat ** Vect m a)
dropFirst Z     Nil     = (Z ** Nil)
dropFirst (S k) (x::xs) = (k ** xs)

splitArray : (n, m, k : Nat) -> Vect k a -> Maybe (Vect n a, Vect m a)
splitArray n m k array with (Equality.decEq (n + m) k)
	splitArray n m k array | Yes prf = Just $ Vect.splitAt n $ rewrite prf in array
	splitArray n m k array | No  _   = Nothing

export
find : Ord a => (n: Nat) -> Vect n a -> a -> Maybe (Fin n)
find 0 Nil   _     = Nothing
find n array value =
    let leftLen = div n 2
        rightLen = leftLen + mod n 2
	in
	case natToFin leftLen n of
	Nothing => Nothing
	Just leftLenFin => 
        let idxValue = Vect.index leftLenFin array 
        in if value == idxValue
		   then Just leftLenFin 
		   else case splitArray leftLen rightLen n array of
				Nothing => Nothing
				Just (left, right) =>
					if value < idxValue
					then findLeft leftLen left value
					else findRight leftLen rightLen right value
	where
		findLeft : (leftLen : Nat)
		            -> Vect leftLen a
		            -> a
		            -> Maybe (Fin n)
		findLeft leftLen left value = 
            do
            resultLeft    <- find leftLen left value
            resultLeftLTE <- Fin.maybeLTE leftLen n
            pure $ Fin.weakenLTE resultLeft resultLeftLTE
		
		findRight : (leftLen, rightLen : Nat)
		            -> Vect rightLen a
		            -> a
		            -> Maybe (Fin n)
		findRight leftLen rightLen right value =
			let (k ** xs) = dropFirst rightLen right
            in case find k xs value of
			   Nothing => Nothing
			   Just resultRight =>
                 Fin.natToFin (Fin.finToNat resultRight + leftLen + 1) n