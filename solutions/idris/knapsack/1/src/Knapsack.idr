module Knapsack

import Data.SortedMap

public export
record Item where
  constructor MkItem
  weight : Int
  value : Int

export
item : Int -> Int -> Item
item weight value = MkItem weight value

export
maximumValue : Int -> List Item -> Int
maximumValue maximumWeight items = tryJump items maximumWeight
    where
    
    tryJump : List Item -> Int -> Int
    tryJump Nil _ = 0 
    tryJump array@(x::xs) maxWeight = 
        if x.weight <= maxWeight
        then let on = x.value + tryJump xs (maxWeight - x.weight)
                 off = tryJump xs (maxWeight) in
                 max on off
            else tryJump xs maxWeight