import XMonad

import XMonad.Hooks.DynamicLog          (dynamicLogWithPP, xmobarPP, shorten, xmobarColor, PP(..))
import XMonad.Hooks.ManageDocks         (manageDocks, avoidStruts)
import XMonad.Hooks.FadeInactive        (fadeInactiveLogHook)

import XMonad.Actions.DynamicWorkspaces (removeWorkspace, addWorkspace)
import XMonad.Actions.GridSelect        (goToSelected, defaultGSConfig)
import XMonad.Actions.Search            (promptSearch, intelligent, multi)

import XMonad.Layout.Accordion          (Accordion(..))
import XMonad.Layout.NoBorders          (smartBorders)
import XMonad.Layout.BoringWindows      (boringAuto, focusUp, focusDown, focusMaster)

import XMonad.Util.Run                  (spawnPipe)
import XMonad.Util.EZConfig             (additionalKeysP)
import XMonad.Util.NamedScratchpad      ( NamedScratchpad(..), customFloating
                                        , namedScratchpadAction, namedScratchpadManageHook)
import qualified XMonad.StackSet as W
import XMonad.Prompt ( XPrompt(..), greenXPConfig, XPConfig(..)
                     , mkXPrompt, mkComplFunFromList, XPPosition(..)
                     )
import System.IO (hPutStrLn)
import Data.List (isPrefixOf)

-- from lib
import Common ( myTerminal, runTerminal
              , myFont
              , fg, bg
              )

import Topics (promptedGoto, promptedShift, myTopicNames)

-- ScratchPads --------------------------------------------------------------

scratchPads = [ NS "htop"     spawnHtop     findHtop     manageHtop
              , NS "terminal" spawnTerminal findTerminal manageTerminal
              ]
  where -- htop settings
        spawnHtop  = runTerminal "htop" $ Just "htop"
        findHtop   = fmap ("htop" `isPrefixOf`) title
        manageHtop = customFloating $ W.RationalRect l t w h
          where h = 0.7
                w = 0.8
                t = (1 - h)/2
                l = (1 - w)/2

        -- terminal settings
        spawnTerminal  = runTerminal "scratchpad" Nothing
        findTerminal   = resource =? "scratchpad"
        manageTerminal = customFloating $ W.RationalRect l t w h
          where h = 0.1
                w = 1
                t = 1 - h
                l = 1 - w

-- Key Bindings --------------------------------------------------------------

myKeyBindings = [ ("M-t"            , promptedGoto)
                , ("M-S-t"          , promptedShift)
                , ("M-g"            , goToSelected defaultGSConfig)
                , ("M-s"            , promptSearch myXPConfig $ intelligent multi)
                  
                , ("M-S-<Backspace>", scratchHtop)
                , ("M-S-r"          , spawn "xmonad --recompile && xmonad --restart")
                , ("M-+"            , newWorkspace)
                , ("M--"            , removeWorkspace)

                , ("M-k"            , focusDown)
                , ("M-j"            , focusUp)
                , ("M-m"            , focusMaster)
                ]
  where scratchHtop     = namedScratchpadAction scratchPads "htop"

-- Key Bindings --------------------------------------------------------------

myLogHook xmproc = dynamicLogWithPP xmobarPP
                 { ppOutput = hPutStrLn xmproc
                 , ppTitle  = xmobarColor "#cccccc" "" . shorten 50
                 , ppHidden = noScratchPad
                 , ppHiddenNoWindows = const ""
                 }
  where noScratchPad ws = if ws == "NSP" then "" else ws
        
-- Layout --------------------------------------------------------------------

tiled = Tall nmaster delta ratio
  where nmaster = 1
        ratio   = 70/100
        delta   =  1/50
        
myLayout = smartBorders
           $ boringAuto
           $ tiled ||| Mirror tiled ||| Accordion ||| Full

-- Prompt --------------------------------------------------------------------

data Prom = Prom String
instance XPrompt Prom where
  showXPrompt (Prom x) = x

myXPConfig = greenXPConfig { font = myFont, fgColor = fg, bgColor = bg, fgHLight = bg, bgHLight = fg, position = Top }
newWorkspace = mkXPrompt (Prom "Name for Workspace: ") myXPConfig (mkComplFunFromList []) addWorkspace

main = do
    spawn "xcompmgr"
    -- spawn "urxvtd -f"
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { borderWidth = 0
        , terminal = myTerminal
        , normalBorderColor = "#000000"
        -- , focusedBorderColor = "#cd8b00"
        -- , focusedBorderColor = "#391529"
        , focusedBorderColor = "#AF0000"
        -- rebind mod to the windows key
        -- , modMask = mod4Mask
        , workspaces = myTopicNames
        , manageHook =   manageDocks
                     <+> manageHook defaultConfig
                     <+> namedScratchpadManageHook scratchPads
        , layoutHook = avoidStruts myLayout
        , logHook    = fadeInactiveLogHook 0.65 >> myLogHook xmproc
        } `additionalKeysP` myKeyBindings
    
