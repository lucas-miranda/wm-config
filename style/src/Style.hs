{-# LANGUAGE TupleSections #-}

module Style (
        processApplyStyle, processApplyStyle', applyStyle,
        StyleConfig (..),
        def, withAlternateColorSections,
        defaultStyleColors,
        Operator,
        runOperator, liftState,
        reverseSections, alternateColorSections,
    ) where

import Control.Monad.Trans.Reader
import Control.Monad.State.Strict
import Control.Applicative
import Style.Face
import Style.Section
import Style.Section.Divider

--------------------------------------

-- | Apply style, as defined at `StyleConfig`, to `Sections` and process every `Section` into a single value
processApplyStyle :: StyleConfig -> Sections -> String
processApplyStyle c s =
     let sections = applyStyle c s
     in concat $ intercalateDividers (map processSection sections) (processDividers c sections)
   where
     processDividers :: StyleConfig -> Sections -> [String]
     processDividers c [] = []
     processDividers c [x] = [processDivider (bgColor x) (externalBgColor c) (dividerConfig c)]
     processDividers c (x1 : x2 : xs) = processDivider (bgColor x1) (bgColor x2) (dividerConfig c) : processDividers c (x2 : xs)

-- | Apply style at reverse order, as defined at `StyleConfig`, to `Sections` and process every `Section` into a single value
processApplyStyle' :: StyleConfig -> Sections -> String
processApplyStyle' c s =
     let sections = applyStyle c (reverse s)
     in concat $ intercalateDividers (reverse $ processDividers c sections) (reverse $ map processSection sections)
   where
     processDividers :: StyleConfig -> Sections -> [String]
     processDividers c [] = []
     processDividers c [x] = [processDivider (externalBgColor c) (bgColor x) (dividerConfig c)]
     processDividers c (x1 : x2 : xs) = processDivider (bgColor x2) (bgColor x1) (dividerConfig c) : processDividers c (x2 : xs)

intercalateDividers :: [String] -> [String] -> [String]
intercalateDividers [] ds = []
intercalateDividers [x] [] = [x]
intercalateDividers [x] (d : ds) = [x, d]
intercalateDividers (x : xs) (d : ds) = x : d : intercalateDividers xs ds
intercalateDividers _ _ = []

-- | Apply style, as defined at `StyleConfig`, to `Sections` and return it
applyStyle :: StyleConfig -> Sections -> Sections
applyStyle c s = snd $ runOperator c s (handleSections c)

--------------------------------------

data StyleConfig = StyleConfig { styleFgColors :: [Color]
                               , styleBgColors :: [Color]
                               , externalBgColor :: Color
                               , dividerConfig :: DividerConfig
                               , handleSections :: Operator ()
                               }

def :: StyleConfig
def = StyleConfig { styleFgColors = [""]
                  , styleBgColors = [""]
                  , externalBgColor = ""
                  , dividerConfig = defSimple
                  , handleSections = pure ()
                  }

withAlternateColorSections :: StyleConfig -> StyleConfig
withAlternateColorSections c = c { handleSections = handleSections c >> alternateColorSections
                                 }

defaultStyleColors :: StyleConfig -> StyleConfig
defaultStyleColors c = c { styleFgColors = ["#B8B8B9"]
                         , styleBgColors = ["#303030",  "#46474F"]
                         , externalBgColor = "#303030"
                         }

{-
defaultStyleConfig :: StyleConfig
defaultStyleConfig = defaultStyleColors
    $ def { dividerConfig =
        DetailedConfig { symbol = "\57534"
                       , spacesAtLeft = 1
                       , spacesAtRight = 1
                       }
          , handleSections = pure ()
          }

, leftRightDividerConfig =
    DetailedConfig { symbol = "\57532"
                   , spacesAtLeft = 1
                   , spacesAtRight = 1
                   }
-}

--------------------------------------

--  Describes an Operation over something
--newtype Operation a = Operation a

--------------------------------------

-- | Describes an operation over Sections (state) with possibility to read from StyleConfig
newtype Operator a = Operator (ReaderT StyleConfig (State Sections) a)

runOperator :: StyleConfig -> Sections -> Operator a -> (a, Sections)
runOperator c s (Operator m) = runState (runReaderT m c) s

liftReader :: ReaderT StyleConfig (State Sections) a -> Operator a
liftReader = Operator

liftState :: (StyleConfig -> State Sections a) -> Operator a
liftState s = Operator $ ReaderT s

instance Functor Operator where
    fmap f as = f <$> as

instance Applicative Operator where
    pure a = Operator . ReaderT $ \ r -> state (a,)
    x <*> y = do
        f <- x
        f <$> y

instance Monad Operator where
    Operator m >>= k = Operator $ ReaderT $ \ r -> do
        let q = runReaderT m r
        state (innerState q r) where
            innerState a r s = do
                let (x, s') = runState a s
                runOperator r s' (k x)

--------------------------------------
-- | Operator's StyleConfig interface

--askStyleConf :: Operator (ReaderT StyleConfig (State Sections) StyleConfig)
--askStyleConf = Operator ask

--asksStyleConf :: (StyleConfig -> a) -> Operator (ReaderT StyleConfig (State Sections) a)
--asksStyleConf = return . asks

--------------------------------------
-- | Operator's Sections interface

getSections :: Operator Sections
getSections = liftState $ const get

putSections :: Sections -> Operator ()
putSections s = liftState $ \ r -> put s

--------------------------------------

reverseSections :: Operator ()
reverseSections = do
    sections <- getSections
    putSections $ reverse sections

alternateColorSections :: Operator ()
alternateColorSections = do
    conf <- Operator ask
    sections <- getSections
    let fgColors = cycle $ styleFgColors conf
    let bgColors = cycle $ styleBgColors conf
    putSections $ map handleSection (zip3 sections fgColors bgColors) where
        handleSection (s, fg, bg) = applyColor s fg bg
