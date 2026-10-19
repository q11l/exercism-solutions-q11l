module TwoFer

export
twoFer : Maybe String -> String
twoFer name = case name of
              Nothing => "One for you, one for me."
              (Just name) => "One for " ++ name ++ ", one for me."