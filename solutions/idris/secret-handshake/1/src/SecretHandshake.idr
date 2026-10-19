module SecretHandshake
import Data.Bits as B

public export
data Action = Wink | DoubleBlink | CloseYourEyes | Jump

tt : Int -> Fin 64 -> Maybe Action
tt num idx = if B.testBit num idx
             then case idx of
                  0 => Just Wink
                  1 => Just DoubleBlink
                  2 => Just CloseYourEyes
                  3 => Just Jump
                  _ => Nothing
             else Nothing

export
commands : Int -> List Action
commands number = 
    if B.testBit number 4
    then mapMaybe (tt number) [3, 2, 1, 0]
    else mapMaybe (tt number) [0, 1, 2, 3]