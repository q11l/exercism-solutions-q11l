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
maximumValue maximumWeight items = knapsack items maximumWeight
    where
    
    knapsack : List Item -> Int -> Int
    knapsack Nil _ = 0 
    knapsack array@(x::xs) maxWeight = 
        if x.weight <= maxWeight
        then let on = x.value + knapsack xs (maxWeight - x.weight)
                 off = knapsack xs (maxWeight) in
                 max on off
            else knapsack xs maxWeight