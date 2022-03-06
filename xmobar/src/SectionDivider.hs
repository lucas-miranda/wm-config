module SectionDivider (
        DividerConfig (..),
        divider
    ) where

data DividerConfig = DividerConfig { symbol :: String
                                   , spacesAtLeft :: Int
                                   , spacesAtRight :: Int
                                   }

divider :: String -> String -> DividerConfig -> String
divider fromColor toColor c = concat [ extraSpace fromColor (spacesAtLeft c)
                                     , "<fc=", toColor, ",", fromColor, ">"
                                     , symbol c
                                     , "</fc>"
                                     , extraSpace toColor (spacesAtRight c)
                                     ]

extraSpace :: String -> Int -> String
extraSpace c n | n > 0 = concat [ "<fc=", ",", c, ">", replicate n ' ', "</fc>" ]
               | otherwise = ""
