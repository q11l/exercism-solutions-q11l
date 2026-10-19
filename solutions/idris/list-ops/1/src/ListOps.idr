module ListOps

export
append : List a -> List a -> List a
append Nil      list2 = list2
append list1    Nil   = list1
append (x::Nil) list2 = x :: list2
append (x::xs)  list2 = (x :: ListOps.append xs list2)

export
concat : List (List a) -> List a
concat Nil     = []
concat (x::xs) = ListOps.append x $ ListOps.concat xs

export
filter : (a -> Bool) -> List a -> List a
filter _         Nil     = []
filter predicate (x::xs) = 
    if predicate x
    then x :: ListOps.filter predicate xs 
    else ListOps.filter predicate xs

export
length : List a -> Nat
length Nil     = 0
length (_::xs) = 1 + ListOps.length xs

export
map : (a -> b) -> List a -> List b
map _        Nil     = []
map function (x::xs) = function x :: ListOps.map function xs 

export
foldl : (a -> e -> a) -> a -> List e -> a
foldl _        initial Nil     = initial
foldl function initial (x::xs) = ListOps.foldl function (function initial x) xs

export
foldr : (a -> e -> a) -> a -> List e -> a
foldr _        initial Nil      = initial
foldr function initial (x::xs)  = function (ListOps.foldr function initial xs) x

export
reverse : List a -> List a
reverse Nil     = []
reverse (x::xs) = append (ListOps.reverse xs) [x]