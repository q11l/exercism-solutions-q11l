module BookStore

import Data.Vect
import Data.Fin

Books : Type
Books = Vect 6 Nat

BookIndexes : Type
BookIndexes = List (Fin 6) 

Cache : Type
Cache = List (Books, Nat)

getPrice : Nat -> Nat
getPrice 1 = 800
getPrice 2 = 1520
getPrice 3 = 2160
getPrice 4 = 2560
getPrice 5 = 3000
getPrice _ = 0

getOneBook : Fin 6 -> Books -> (Books, Nat)
getOneBook index books = 
    if 0 < Vect.index index books
    then (updateAt index (`minus` 1) books, 1)
    else (books, 0)

getDifferentBooks : Books -> BookIndexes -> (Books, Nat)
getDifferentBooks books Nil = (books, 0)
getDifferentBooks books (index :: indexes) = 
    let (newBook, bit) = getOneBook index books 
        (nextBook, nextNum) = getDifferentBooks newBook indexes in
    (nextBook, bit + nextNum)

minNoZero : Nat -> Nat -> Nat
minNoZero 0 y = y
minNoZero x 0 = x
minNoZero x y = if x > y then y else x

getSubGroups : Vect n Nat -> BookIndexes -> List BookIndexes
getSubGroups (y :: ys) (index :: indexes) =
    let next = getSubGroups ys indexes in
        if  0 < y
        then ((index ::) <$> next) ++ next
        else next
getSubGroups _ _ = [[]]

compareWithCache : Books -> Cache -> Maybe Nat
compareWithCache books Nil = Nothing
compareWithCache books ((cacheBooks, number) :: cache) =
    if cacheBooks == books
    then Just number
    else compareWithCache books cache

getMaxDiscount : Books -> Cache -> (Nat, Cache)
getMaxDiscount books cache = 
    case compareWithCache books cache of
    Just number => (number, cache)
    Nothing  => 
        let (number, newCache) = foldWithCache cache (accumulatePrice books) (getSubGroups books [0, 1, 2, 3, 4, 5]) in
        (number, (books, number) :: newCache)
    where
        accumulatePrice : Books -> Cache -> BookIndexes -> (Nat, Cache)
        accumulatePrice _ cache Nil = (0, cache)
        accumulatePrice books cache indexes = 
            let (newBooks, number) = getDifferentBooks books indexes 
                price = getPrice number in
                if number == 0 then (0, cache) else
                    let (newNumber, newCache) = getMaxDiscount newBooks cache in
                      (price + newNumber, newCache)
                
        foldWithCache : Cache 
                    -> (Cache -> BookIndexes -> (Nat, Cache)) 
                    -> List BookIndexes 
                    -> (Nat, Cache)
        foldWithCache cache func Nil = (0, cache)
        foldWithCache cache func (index :: indexes) = 
            let (number, subCache) = func cache index 
                (newNumber, newCache) = foldWithCache subCache func indexes in
            (minNoZero number newNumber, newCache)
            
countBooks : BookIndexes -> Books -> Books
countBooks Nil books = books
countBooks (index :: indexes) books = countBooks indexes $ updateAt index (+1) books  

export
price : List (Fin 6) -> Nat
price books = 
    let (number, _) = getMaxDiscount (countBooks books [0, 0, 0, 0, 0, 0]) [] in
    number