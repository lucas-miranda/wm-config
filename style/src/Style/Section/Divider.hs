{-# LANGUAGE NamedFieldPuns #-}

module Style.Section.Divider (
        DividerConfig (..),
        defDetailed, defSimple,
        processDivider,
    ) where

import Style.Face

data DividerConfig = DetailedConfig
                       { symbol :: String
                       , spacesAtLeft :: Int
                       , spacesAtRight :: Int
                       }
                   | SimpleConfig String

defDetailed :: DividerConfig
defDetailed = DetailedConfig { symbol = "|"
                             , spacesAtLeft = 1
                             , spacesAtRight = 1
                             }

defSimple :: DividerConfig
defSimple = SimpleConfig "|"

processDivider :: Color -> Color -> DividerConfig -> String
processDivider fromColor toColor (SimpleConfig symbol) = color toColor fromColor symbol
processDivider
    fromColor toColor
    DetailedConfig { symbol
                   , spacesAtLeft
                   , spacesAtRight
                   } =
        concat [ extraSpace fromColor spacesAtLeft
               , color toColor fromColor symbol
               , extraSpace toColor spacesAtRight
               ]

extraSpace :: Color -> Int -> String
extraSpace c n | n > 0 = colorBg c (replicate n ' ')
               | otherwise = ""
