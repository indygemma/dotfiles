import XMonad

import XMonad.Hooks.DynamicLog          (dynamicLogWithPP, xmobarPP, shorten, xmobarColor, PP(..))
import XMonad.Hooks.ManageDocks         (manageDocks, avoidStruts)
import XMonad.Hooks.FadeInactive        (fadeInactiveLogHook)

import XMonad.Actions.DynamicWorkspaces (removeWorkspace, addWorkspace, renameWorkspace)
import XMonad.Actions.GridSelect        (goToSelected, defaultGSConfig)
import XMonad.Actions.Search            (promptSearch, intelligent, multi)

import XMonad.Layout.Accordion          (Accordion(..))
import XMonad.Layout.NoBorders          (smartBorders)
import XMonad.Layout.BoringWindows      (boringAuto, focusUp, focusDown, focusMaster)
import XMonad.Layout.Renamed            (renamed, Rename(..))

import XMonad.Util.Run                  (spawnPipe)
import XMonad.Util.EZConfig             ( additionalKeys, additionalKeysP)
import XMonad.Util.NamedScratchpad      ( NamedScratchpad(..), customFloating
                                        , namedScratchpadAction, namedScratchpadManageHook)
import XMonad.Prompt ( XPrompt(..), greenXPConfig, XPConfig(..)
                     , mkXPrompt, mkComplFunFromList, XPPosition(..)
                     )
import System.IO (hPutStrLn)
import Data.List (isPrefixOf)

import qualified XMonad.StackSet as W
import qualified Graphics.X11.ExtraTypes.XF86 as XF86

-- from lib
import Common ( myTerminal, runTerminal
              , myFont
              , fg, bg
              )
import Topics (promptedGoto, promptedShift, myTopicNames)
import Spacing (SPACING(SPACING), spacing)
import qualified BinarySpacePartition as BSP

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
                , ("M-n"            , renameWorkspace myXPConfig)

                -- dynamic spacing
                , ("M-C-+"          , sendMessage $ SPACING 5)
                , ("M-C--"          , sendMessage $ SPACING $ negate 5)

                , ("M-k"            , focusDown)
                , ("M-j"            , focusUp)
                , ("M-m"            , focusMaster)
                ]
  where scratchHtop     = namedScratchpadAction scratchPads "htop"

mediaKeyBindings = [
  -- audio media keys
    ((0, XF86.xF86XK_AudioMute ), spawn "amixer -D pulse set Master toggle" )
  , ((0, XF86.xF86XK_AudioLowerVolume ), spawn "amixer -D pulse set Master 10%-" )
  , ((0, XF86.xF86XK_AudioRaiseVolume ), spawn "amixer -D pulse set Master 10%+" )

  -- screen brightness keys
  , ((0, XF86.xF86XK_MonBrightnessDown ), spawn "xcalib -co 50 -a" )
  , ((0, XF86.xF86XK_MonBrightnessUp ),   spawn "xcalib -c" )

  -- keyboard brightness keys
  , ((0, XF86.xF86XK_KbdBrightnessDown ), spawn "kbdlight down" )
  , ((0, XF86.xF86XK_KbdBrightnessUp ),   spawn "kbdlight up" )
  ]

-- Log Hook  --------------------------------------------------------------

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
           $ renamed [CutWordsLeft 2] -- Remove "Spacing 15" in Layout title
           $ spacing 15
           $   BSP.emptyBSP
           ||| tiled
           ||| (Mirror tiled)
           ||| Accordion
           ||| Full

-- Prompt --------------------------------------------------------------------

data Prom = Prom String
instance XPrompt Prom where
  showXPrompt (Prom x) = x

myXPConfig = greenXPConfig { font = myFont, fgColor = fg, bgColor = bg, fgHLight = bg, bgHLight = fg, position = Top }
newWorkspace = mkXPrompt (Prom "Name for Workspace: ") myXPConfig (mkComplFunFromList []) addWorkspace

main = do
    spawn "xcompmgr"
    spawn "feh --bg-max --randomize ~/dotfiles/wallpapers/*"
    --spawn "urxvtd -f"
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { borderWidth = 1
        , terminal = myTerminal
        , normalBorderColor = "#000000"
        -- , focusedBorderColor = "#cd8b00"
        -- , focusedBorderColor = "#391529"
        , focusedBorderColor = "#1B6371"
        -- rebind mod to the windows key
        , modMask = mod4Mask
        , workspaces = myTopicNames
        , manageHook =   manageDocks
                     <+> manageHook defaultConfig
                     <+> namedScratchpadManageHook scratchPads
        , layoutHook = avoidStruts myLayout
        , logHook    = fadeInactiveLogHook 0.90 >> myLogHook xmproc
        } `additionalKeysP` myKeyBindings
          `additionalKeys`  mediaKeyBindings

