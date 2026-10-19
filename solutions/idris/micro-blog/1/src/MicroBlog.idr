module MicroBlog
import Data.List as L

export
truncate : String -> String
truncate = pack . L.take 5 . unpack