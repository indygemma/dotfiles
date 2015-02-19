module Common (
    myTerminal
 , runTerminal
  , myFont
  , fg
  , bg
)where

myTerminal :: String
myTerminal = "urxvtc"

runTerminal :: String -> Maybe String -> String
runTerminal title mCmd = case mCmd of
  Just cmd -> myTerminal ++ " -title " ++ title ++ " -e " ++ cmd
  Nothing  -> myTerminal ++ " -title " ++ title

myFont :: String
myFont = "xft:Liberation Mono:size=8:antialias=true"

fg :: String
fg = "#839496"

bg :: String
bg = "#002b36"
