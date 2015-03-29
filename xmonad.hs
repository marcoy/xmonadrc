import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Layout.Maximize
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W

import qualified Data.Map as M

myModMask  = mod1Mask -- mod4Mask
myTerminal = "lilyterm"
myBrowser  = "chromium"

myManageHook = composeAll . concat $
        [ [className =? f  --> doFloat      | f <- myFloats]
        , [isFullscreen    --> doFullFloat                 ]
        ]
        where
            myFloats = ["Vlc", "Vncviewer"]

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
myLayout = toggleLayouts Full (maximize tiled) ||| Mirror tiled ||| tabbed shrinkText defaultTheme ||| Full
    where
        tiled = ResizableTall 1 (3/100) (1/2) []

-- Keys
toggleStrutsKey' XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myKeys = [ ((myModMask                 , xK_space    ), sendMessage NextLayout)
         , ((myModMask                 , xK_Return   ), spawn $ XMonad.terminal myConfig)
         , ((myModMask .|. shiftMask   , xK_Return   ), safeSpawn myBrowser [])
         , ((myModMask .|. controlMask , xK_Return   ), windows W.swapMaster)
         , ((myModMask .|. controlMask , xK_space    ), sendMessage ToggleLayout)
         , ((myModMask                 , xK_a        ), sendMessage MirrorExpand)
         , ((myModMask                 , xK_z        ), sendMessage MirrorShrink)
         , ((myModMask                 , xK_backslash), withFocused (sendMessage . maximizeRestore))
         ]
