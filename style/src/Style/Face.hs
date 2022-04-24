
module Style.Face (
        Color,
        color, colorBg, colorFg
    ) where

type Color = String

color :: Color -> Color -> String -> String
color fg bg text = concat ["<fc=", fg, ",", bg, ">", text, "</fc>"]

colorBg :: Color -> String -> String
colorBg bg text = concat ["<fc=", ",", bg, ">", text, "</fc>"]

colorFg :: Color -> String -> String
colorFg fg text = concat ["<fc=", fg, ",", ">", text, "</fc>"]
