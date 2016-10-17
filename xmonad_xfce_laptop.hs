-------------------------------------------------------------------------------
-- Configuration for using xmonad inside xfce4.
-- Credits: Johannes 'wulax' Sjölund and Øyvind 'Mr.Elendig' Heggstad
-- https://gist.github.com/jsjolund/94f6821b248ff79586ba
-------------------------------------------------------------------------------

import qualified Data.Map as M

import qualified XMonad.StackSet as W
import Control.Exception
import System.Exit

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.ComboP
import XMonad.Layout.ResizableTile


conf = ewmh xfceConfig
	{ manageHook        = pbManageHook <+> manageDocks
					   <+> manageHook xfceConfig
	, handleEventHook   = ewmhDesktopsEventHook
	, borderWidth       = 3
	, focusedBorderColor= "#93E0E3"
	, normalBorderColor = "#444444"
	, workspaces        = map show [1 .. 9 :: Int]
	, modMask           = mod4Mask
	, keys              = myKeys
	 }


-- Main --
main :: IO ()
main =
    xmonad $ conf
	{ startupHook       = startupHook conf
	, logHook           =  ewmhDesktopsLogHook
	 }


-- ManageHook --
pbManageHook :: ManageHook
pbManageHook = composeAll $ concat
    [ [ manageDocks ]
    , [ manageHook defaultConfig ]
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> doFullFloat ]
    ]


-- Keyboard --
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,                xK_Return   ), spawn "terminator")
    , ((modMask,                xK_o        ), spawn "xfrun4")
    , ((modMask,                xK_f        ), spawn "thunar")
    , ((modMask,                xK_c        ), kill)
    , ((modMask,                xK_b        ), sendMessage ToggleStruts)

    -- layouts
    , ((modMask,                xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,  xK_space    ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask,                xK_t        ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,                xK_n        ), refresh)

    -- focus
    , ((modMask,                xK_Tab      ), windows W.focusDown)
    , ((modMask,                xK_j        ), windows W.focusDown)
    , ((modMask,                xK_k        ), windows W.focusUp)
    , ((modMask,                xK_m        ), windows W.focusMaster)
    , ((modMask,                xK_Right    ), nextWS)
    , ((modMask,                xK_Left     ), prevWS)
    , ((modMask .|. shiftMask,  xK_Right    ), shiftToNext >> nextWS)
    , ((modMask .|. shiftMask,  xK_Left     ), shiftToPrev >> prevWS)

    -- swapping
    , ((modMask .|. shiftMask,  xK_Return   ), windows W.swapMaster)
    , ((modMask .|. shiftMask,  xK_j        ), windows W.swapDown)
    , ((modMask .|. shiftMask,  xK_k        ), windows W.swapUp)
    , ((modMask,                xK_s        ), sendMessage $ SwapWindow)

    -- increase or decrease number of windows in the master area
    , ((modMask,                xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                xK_period   ), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,                xK_h        ), sendMessage Shrink)
    , ((modMask,                xK_l        ), sendMessage Expand)
    , ((modMask .|. shiftMask,  xK_h        ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask,  xK_l        ), sendMessage MirrorExpand)

    -- quit, or restart
    , ((modMask .|. shiftMask,  xK_q        ), spawn "xmonad --recompile; xmonad --restart")
    , ((modMask,                xK_q        ), spawn "xfce4-session-logout")
    , ((modMask .|. shiftMask,  xK_z        ), spawn "xscreensaver-command --lock")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [ ((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
