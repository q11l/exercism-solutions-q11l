module RnaTranscription

import Data.Vect

%default total

namespace DnaNucleotide
  public export
  data DnaNucleotide = G | C | T | A

namespace RnaNucleotide
  public export
  data RnaNucleotide = C | G | A | U

export
implementation Eq RnaNucleotide where
  (==) RnaNucleotide.C RnaNucleotide.C = True
  (==) RnaNucleotide.G RnaNucleotide.G = True
  (==) RnaNucleotide.A RnaNucleotide.A = True
  (==) RnaNucleotide.U RnaNucleotide.U = True
  (==) _ _ = False

export
implementation Show RnaNucleotide where
  show RnaNucleotide.C = "C"
  show RnaNucleotide.G = "G"
  show RnaNucleotide.A = "A"
  show RnaNucleotide.U = "U"

export
toRna : Vect n DnaNucleotide -> Vect n RnaNucleotide
toRna Nil = []
toRna (x::xs) = getComplement x :: toRna xs where 
                getComplement : DnaNucleotide -> RnaNucleotide
                getComplement x = case x of
                                    G => C
                                    C => G
                                    T => A
                                    A => U