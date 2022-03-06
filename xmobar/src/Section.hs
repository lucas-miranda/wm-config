module Section (
        SectionConfig (..),
        simple
    ) where

data SectionConfig = SectionConfig { foregroundColor :: String
                                   , backgroundColor :: String
                                   }

simple :: String -> String -> String -> String
simple fg bg c = concat ["<fc=", fg, ",", bg, ">", c, "</fc>"]
