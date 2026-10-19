module ResistorColorTrio
import Data.Vect as V

Func : Type
Func = Either (Integer -> Integer -> Integer) (Integer -> Integer -> Double)

colors : Vect 10 String
colors = [
            "black",
            "brown",
            "red",
            "orange",
            "yellow",
            "green",
            "blue",
            "violet",
            "grey",
            "white"
         ]

getSufix : Integer -> Integer -> Fin 10 -> Maybe (String, Func)
getSufix c1 c2 c3 = 
    let f1 : Func = Left  $ \x, y => x * 10 + y
        f2 : Func = Left  $ \x, y => x * 100 + y * 10
        f3 : Func = if c2 == 0 
                    then Left  $ \x, y => div (x * 10 + y) 10
                    else Right $ \x, y => (cast (x * 10 + y)) / 10.0
    in case c3 of
            0 => Just ("ohms",     f1)
            1 => Just ("ohms",     f2)
            2 => Just ("kiloohms", f3)
            3 => Just ("kiloohms", f1)
            4 => Just ("kiloohms", f2)
            5 => Just ("megaohms", f3)
            6 => Just ("megaohms", f1)
            7 => Just ("megaohms", f2)
            8 => Just ("gigaohms", f3)
            9 => Just ("gigaohms", f1)
            _ => Nothing

export
label : Vect 3 String -> Maybe String
label [x, y, z] = do
                  c1 <- (V.elemIndex x colors)
                  c2 <- (V.elemIndex y colors)
                  c3 <- (V.elemIndex z colors)
                  let (c11, c22) : (Integer , Integer) = (cast c1, cast c2)
                  (sufix, func) <- getSufix c11 c22 c3
                  pure $ case func of
                         Left  f => "\{show $ f c11 c22} \{sufix}"
                         Right f => "\{show $ f c11 c22} \{sufix}"