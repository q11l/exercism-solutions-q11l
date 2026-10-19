module Meetup
import Data.Vect
import Data.Fin

public export
data Month = January | February | March | April | May | June | July | August | September | October | November | December

public export
data Week = First | Second | Third | Fourth | Teenth | Last

public export
data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

getNumMonth : Month -> Int
getNumMonth January   = 1
getNumMonth February  = 2
getNumMonth March     = 3
getNumMonth April     = 4
getNumMonth May       = 5
getNumMonth June      = 6
getNumMonth July      = 7
getNumMonth August    = 8
getNumMonth September = 9
getNumMonth October   = 10
getNumMonth November  = 11
getNumMonth December  = 12

getNumDaysOfWeek : DayOfWeek -> Int
getNumDaysOfWeek Monday    = 1
getNumDaysOfWeek Tuesday   = 2
getNumDaysOfWeek Wednesday = 3
getNumDaysOfWeek Thursday  = 4
getNumDaysOfWeek Friday    = 5
getNumDaysOfWeek Saturday  = 6
getNumDaysOfWeek Sunday    = 7

isLeap : Nat -> Bool
isLeap year =
  case (mod year 4, mod year 400, mod year 100) of
  (0, 0, 0) => True
  (0, _, 0) => False
  (0, _, _) => True
  _ => False

getDaysOfMonths : Nat -> Vect 12 Int
getDaysOfMonths year = [ 31, if isLeap year then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

add0 : Int -> String
add0 x =
  if x < 10
  then "0" ++ show x
  else show x

addLeaps : List Nat -> Int
addLeaps Nil = 0
addLeaps (x::xs) =
  if isLeap x
  then 2 + addLeaps xs
  else 1 + addLeaps xs

getTotalDays : Fin n -> Vect m Int -> Int
getTotalDays (FS k) (x::xs) = x + getTotalDays k xs
getTotalDays _ _ = 0

getDay : Week -> Int -> Int -> Int
getDay First  _ day = 0  + day
getDay Second _ day = 7  + day
getDay Third  _ day = 14 + day
getDay Fourth _ day = 21 + day
getDay Teenth _ day = 13 + mod (day + 1) 7
getDay Last month day =
  case month of
  28 => 21 + day
  29 => 29 - mod (1 - day) 7
  30 => 30 - mod (2 - day) 7
  31 => 31 - mod (3 - day) 7
  _ => 0

export
meetup : Nat -> Month -> Week -> DayOfWeek -> String
meetup year month week dayofweek =
  let numMonth        = getNumMonth month
      numDaysOfWeek   = getNumDaysOfWeek dayofweek
      vectDaysOfMonth = getDaysOfMonths year
      idx =
        case natToFin (cast $ numMonth - 1) 12 of
        Nothing => 0
        Just a  => a
      totalDays   = getTotalDays idx vectDaysOfMonth
      daysOfMonth = index idx vectDaysOfMonth
      d1 = numDaysOfWeek - mod totalDays 7
      d2 = mod (d1 - addLeaps [1996..minus year 1]) 7
      d3 = if d2 == 0 then 7 else d2
  in
  "\{show year}-\{add0 numMonth}-\{add0 $ getDay week daysOfMonth d3}"