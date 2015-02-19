import XMonad
import XMonad.Hooks.DynamicLog     (dynamicLogWithPP, xmobarPP, shorten, xmobarColor, PP(..))
import XMonad.Hooks.ManageDocks    (manageDocks, avoidStruts)
import XMonad.Util.Run             (spawnPipe)
import XMonad.Util.EZConfig        (additionalKeysP)
import XMonad.Util.NamedScratchpad ( NamedScratchpad(..), customFloating
                                   , namedScratchpadAction, namedScratchpadManageHook)
import qualified XMonad.StackSet as W
import System.IO (hPutStrLn)
import Data.List (isPrefixOf)

myTerminal :: String
myTerminal = "gnome-terminal"

scratchPads = [ NS "htop"     spawnHtop     findHtop     manageHtop
              , NS "terminal" spawnTerminal findTerminal manageTerminal
              ]
  where -- htop settings
        spawnHtop  = myTerminal ++ " -t htop -e htop"
        findHtop   = fmap ("htop" `isPrefixOf`) title
        manageHtop = customFloating $ W.RationalRect l t w h
          where h = 0.7
                w = 0.8
                t = (1 - h)/2
                l = (1 - w)/2

        -- terminal settings
        spawnTerminal  = myTerminal ++ " -t scratchpad"
        findTerminal   = resource =? "scratchpad"
        manageTerminal = customFloating $ W.RationalRect l t w h
          where h = 0.1
                w = 1
                t = 1 - h
                l = 1 - w

myKeyBindings = [ ("M-t", scratchTerminal)
                , ("M-<Backspace>", scratchHtop)
                ]
  where scratchTerminal = namedScratchpadAction scratchPads "terminal"
        scratchHtop     = namedScratchpadAction scratchPads "htop"

myLogHook xmproc = dynamicLogWithPP xmobarPP
                 { ppOutput = hPutStrLn xmproc
                 , ppTitle  = xmobarColor "#cccccc" "" . shorten 50
                 , ppHidden = noScratchPad
                 , ppHiddenNoWindows = const ""
                 }
  where noScratchPad ws = if ws == "NSP" then "" else ws

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { borderWidth = 1
        , terminal = myTerminal
        , normalBorderColor = "#000000"
        -- , focusedBorderColor = "#cd8b00"
        -- , focusedBorderColor = "#391529"
        , focusedBorderColor = "#AF0000"
        -- rebind mod to the windows key
        -- , modMask = mod4Mask
        , manageHook =   manageDocks
                     <+> manageHook defaultConfig
                     <+> namedScratchpadManageHook scratchPads
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook    = myLogHook xmproc
        } `additionalKeysP` myKeyBindings
    
