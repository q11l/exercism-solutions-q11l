module RobotSimulator

import Data.Vect

public export
data Direction = North | East | South | West

public export
data Instruction = R | L | A

public export
record Robot where
  constructor MkRobot
  direction   : Direction
  coordinates : Vect 2 Integer

public export
create : Direction -> Integer -> Integer -> Robot
create direction x y = MkRobot direction [x, y]

Eq Direction where
    North == North = True
    East  == East  = True
    West  == West  = True
    South == South = True
    _     == _     = False

Eq Instruction where
    A == A = True
    L == L = True
    R == R = True
    _ == _ = False
    
ins : Instruction -> Nat
ins A = 0
ins L = 1
ins R = 3

dirs : Vect 4 Direction
dirs = [North, West, South, East]
    
go : Robot -> Instruction -> Maybe Robot 
go robot instruction = 
    let [x, y] = robot.coordinates
        move = [[x, y + 1], [x - 1, y], [x, y - 1], [x + 1, y]] in
        do
        idxDir <- Vect.elemIndex robot.direction dirs 
        dir <- Fin.natToFin (mod (ins instruction + Fin.finToNat idxDir) 4) 4
        Just $ MkRobot (Vect.index dir dirs) $ 
            if instruction == A 
            then Vect.index dir move 
            else [x, y]

loop : Robot -> Instruction -> Robot
loop robot instruction = 
    case go robot instruction of
    Nothing => MkRobot North [0, 0]
    Just  a => a

public export
move : Robot -> List Instruction -> Robot
move robot instructions = foldl loop robot instructions