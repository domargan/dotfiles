import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
 
myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
      , isFullscreen --> doFullFloat
    ]

myFocusedBorderColor = "#93E0E3"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> myManageHook
                        <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "#60B48A" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
	, focusedBorderColor = myFocusedBorderColor
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "dm-tool lock")
	]

