module Style.Section (
        Sections,
        sections,
        Section (..),
        applyColor, applyFgColor, applyBgColor,
        processSection,
    ) where

import Style.Face
import Style.Section.Divider

--------------------------------------

type Sections = [Section]

sections :: [String] -> Sections
sections = map (\ c -> section c "#000000" "#FFFFFF")

--processSections :: Sections -> String
--processSections = concatMap processSection

--------------------------------------

data Section = Section { contents :: String
                       , fgColor :: Color
                       , bgColor :: Color
                       }

section c fg bg = Section { contents = c
                          , fgColor = fg
                          , bgColor = bg
                          }

applyColor :: Section -> Color -> Color -> Section
applyColor s fg bg = s { fgColor = fg
                       , bgColor = bg
                       }

applyFgColor :: Section -> Color -> Section
applyFgColor s fg = s { fgColor = fg }

applyBgColor :: Section -> Color -> Section
applyBgColor s bg = s { bgColor = bg }

processSection :: Section -> String
processSection s = color (fgColor s) (bgColor s) (contents s)

instance Show Section where
    show a = "(contents: " ++ contents a ++ ";\
             \ fg: " ++ fgColor a ++ ";\
             \ bg: " ++ bgColor a ++ ")"

--------------------------------------

{-
data SectionConfig = SectionConfig { foregroundColor :: Color
                                   , backgroundColor :: Color
                                   }
-}

{-
singleSection :: Color -> Color -> DividerConfig -> String -> String
singleSection bgColorA bgColorB divConfig text =
    concat [ divider bgColorB bgColorA divConfig
           , text
           , divider bgColorA bgColorB divConfig
           ]
-}
