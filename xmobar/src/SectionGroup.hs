{-# LANGUAGE NamedFieldPuns #-}

module SectionGroup (
        AnchorSide (..),
        SectionGroupConfig (..),
        SectionGroup.simple
    ) where

import Data.Bool
import Section
import SectionDivider

data AnchorSide = AtLeft
                | AtRight

data SectionGroupConfig = SectionGroupConfig { anchor :: AnchorSide
                                             , dividerConfig :: DividerConfig
                                             , sectionA :: SectionConfig
                                             , sectionB :: SectionConfig
                                             }

-- | Creates section using simple input (string)
simple :: SectionGroupConfig -> [String] -> String
simple SectionGroupConfig { anchor = AtLeft } l = ""
simple SectionGroupConfig { anchor = AtRight, dividerConfig, sectionA, sectionB } l =
    handleGroup (splitList $ reverse l) sectionA sectionB where
        -- | Handle intercalated group, by using splitted sections and it's configurations
        handleGroup (a, b) sA sB =
            concat . prependDivider (createDivider sA sB) (createDivider sB sA) $
                reverse $ combineIntercalate (map (section sA) a) (map (section sB) b)

        -- | Create a simple section from section config
        section s = Section.simple (foregroundColor s) (backgroundColor s)

        -- | Combine two lists into one by intercalating it's elements
        combineIntercalate []     []     = []
        combineIntercalate (a:as) []     = a : combineIntercalate as []
        combineIntercalate []     (b:bs) = b : combineIntercalate [] bs
        combineIntercalate (a:as) (b:bs) = a : b : combineIntercalate as bs

        -- | Prepend alternating dividers to each element of a list
        prependDivider _ _ [] = []
        prependDivider dA dB (l:ls) = dA : l : prependDivider dB dA ls

        -- | Create a divider which goes from sections a to b
        createDivider a b = divider (backgroundColor a) (backgroundColor b) dividerConfig

-- | Splits a list into a tuple of lists by intercalating it's elements
splitList :: [a] -> ([a], [a])
splitList l = (subSplit 1 l, subSplit 2 l) where
    subSplit n s = map fst . filter (odd . snd) . zip s $ [n..]
