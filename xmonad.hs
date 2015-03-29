import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig (additionalKeys)

import qualified Data.Map as M

myModMask  = mod1Mask -- mod4Mask
myTerminal = "lilyterm"
myBrowser  = "chromium"

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , className =? "Vlc"       --> doFloat
    , isFullscreen             --> doFullFloat
    ]

main :: IO ()
main = xmonad =<< statusBar barCmd pp toggleStrutsKey conf
        where
            barCmd = "/usr/bin/xmobar /home/marcoy/.xmonad/xmobarrc"
            pp     = myPP
            conf   = myConfig
            toggleStrutsKey = toggleStrutsKey'

myPP = defaultPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
                 , ppHidden = xmobarColor "#C98F0A" ""
                 , ppHiddenNoWindows = xmobarColor "#C9A34E" ""
                 , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]" 
                 , ppLayout = xmobarColor "#C9A34E" ""
                 , ppTitle =  xmobarColor "#C9A34E" "" . shorten 55
                 , ppSep = xmobarColor "#429942" "" " | "
                 }

myConfig = ewmh defaultConfig { borderWidth = 1
                              , modMask     = myModMask
                              , terminal    = myTerminal
                              , manageHook  = manageDocks <+> myManageHook <+> manageHook defaultConfig
                              , layoutHook  = avoidStruts  $  myLayout
                              , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
                              } `additionalKeys` myKeys

-- Layout
-- Use `onWorkspaces' to configure layout on different workspaces.
myLayout = tiled ||| Mirror tiled ||| simpleTabbed ||| Full
    where
        tiled = ResizableTall 1 (3/100) (1/2) []

-- Keys
toggleStrutsKey' XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myKeys = [ ((myModMask               , xK_space ), sendMessage NextLayout)
         , ((myModMask               , xK_Return), spawn $ XMonad.terminal myConfig)
         , ((myModMask .|. shiftMask , xK_Return), safeSpawn myBrowser [])
         , ((myModMask               , xK_a     ), sendMessage MirrorExpand)
         , ((myModMask               , xK_z     ), sendMessage MirrorShrink)
         ]
