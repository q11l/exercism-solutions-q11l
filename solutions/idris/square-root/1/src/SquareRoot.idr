module SquareRoot

heron : Int -> Int -> Int
heron x num = let new = div ((x * x) + num) (2 * x)
              in if x == new then new else heron new num

export
squareRoot : Int -> Int
squareRoot radicand = if radicand < 1 then 0 else heron 1 radicand