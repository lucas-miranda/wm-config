module Main (main) where

import Style
import Style.Section
import Style.Section.Divider
import Style (reverseSections, withAlternateColorSections)

{-
sectionGroupConfig :: SectionGroupConfig
sectionGroupConfig = SectionGroupConfig
    { anchor = AtRight
    , dividerConfig = DividerConfig { symbol = "\57534"
                                    , spacesAtLeft = 1
                                    , spacesAtRight = 1
                                    }
    , sectionA = SectionConfig { foregroundColor = "#B8B8B9"
                               , backgroundColor = "#303030"
                               }
    , sectionB = SectionConfig { foregroundColor = "#B8B8B9"
                               , backgroundColor = "#46474F"
                               }
    }

divConfig = DetailedConfig { symbol = "\57534"
                           , spacesAtLeft = 1
                           , spacesAtRight = 1
                           }
-}

{-
--sectionsStyle $ withDiv divConfig . withAlternateBg
sectionsStyle $ withAlternateColor $ defaultStyleConfig
-}

----------------------------------

{-
rightLeftDivConfig = DetailedConfig { symbol = "\57534"
                                    , spacesAtLeft = 1
                                    , spacesAtRight = 1
                                    }

leftRightDivConfig = DetailedConfig { symbol = "\57532"
                                    , spacesAtLeft = 1
                                    , spacesAtRight = 1
                                    }

styleConfig = StyleConfig { styleFgColors = ["#B8B8B9"]
                          , styleBgColors = ["#303030", "#46474F"]
                          , rightLeftDividerConfig = rightLeftDivConfig
                          , leftRightDividerConfig = leftRightDivConfig
                          , handleSections = withAlternateColor
                          }

operations :: Operator ()
operations = do
    return ()
-}

--myStyle = sectionsStyle $ withAlternateColor $ defaultStyleConfig
--myStyle = withAlternateColorSections defaultStyleConfig

rightSideBarStyle :: StyleConfig
rightSideBarStyle = withAlternateColorSections . defaultStyleColors
    $ def { dividerConfig = defDetailed { symbol = "\57534" }
          , handleSections = pure ()
          }

----------------------------------

generateSections :: Sections
generateSections = sections ["aaa", "bbb", "ccc", "ddd"]

main :: IO ()
main = do
    -- create sections
    let secs = generateSections
    print secs

    -- run operations over sections
    print $ applyStyle rightSideBarStyle secs
    --print $ processApplyStyle myStyle secs
    print $ processApplyStyle rightSideBarStyle secs

    return ()
