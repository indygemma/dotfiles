import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig

import qualified Data.Map as M

myModMask = mod1Mask

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-launcher" --> doFloat
    ])

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, xK_p ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    ]

    --
    -- mod-[1..9], Switch to workspace N

main = do
    xmonad $ gnomeConfig {

        manageHook = myManageHook,
    
        borderWidth = 1,
        normalBorderColor = "#dddddd",
        focusedBorderColor = "#ff0000"

    } `additionalKeys` [
            -- open dmenu
            ((myModMask, xK_p ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
        ]

