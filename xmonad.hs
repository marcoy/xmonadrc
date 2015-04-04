import XMonad

import XMonad.Actions.TagWindows

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)

import XMonad.Layout.Maximize
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts

import XMonad.Prompt

import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig (additionalKeys)

import qualified XMonad.StackSet as W
import qualified Data.Map as M

myModMask  = mod4Mask -- mod1Mask
myTerminal = "lilyterm"
myBrowser  = "chromium"

myManageHook = composeAll . concat $
        [ [className =? f  --> doFloat      | f <- myFloats ]
        , [resource  =? i  --> doIgnore     | i <- myIgnores]
        , [isFullscreen    --> doFullFloat                  ]
        ]
        where
            myFloats = ["Vlc", "Vncviewer"]
            myIgnores = ["synapse"]

main :: IO ()
main = xmonad =<< statusBar barCmd pp toggleStrutsKey conf
        where
            barCmd = "/usr/bin/xmobar /home/marcoy/.xmonad/xmobarrc"
            pp     = myPP
            conf   = myConfig
            toggleStrutsKey = toggleStrutsKey'

myConfig = ewmh defaultConfig { borderWidth = 1
                              , modMask     = myModMask
                              , terminal    = myTerminal
                              , manageHook  = manageDocks <+> myManageHook <+> manageHook defaultConfig
                              , layoutHook  = avoidStruts  $  myLayout
                              , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
                              } `additionalKeys` myKeys

-- PrettyPrinting for xmobar
myPP = defaultPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
                 , ppHidden = xmobarColor "#C98F0A" ""
                 , ppHiddenNoWindows = xmobarColor "#C9A34E" ""
                 , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                 , ppLayout = xmobarColor "#C9A34E" ""
                 , ppTitle =  xmobarColor "#C9A34E" "" . shorten 55
                 , ppSep = xmobarColor "#429942" "" " | "
                 }

myXPConfig = defaultXPConfig { position = Top }

-- Layout
-- Use `onWorkspaces' to configure layout on different workspaces.
myLayout = toggleLayouts Full (maximize tiled) ||| Mirror tiled ||| tabbed shrinkText defaultTheme
    where
        tiled = ResizableTall 1 (3/100) (1/2) []

-- Keys
toggleStrutsKey' XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myKeys = [ ((myModMask                 , xK_space    ), sendMessage NextLayout)
         , ((myModMask                 , xK_Return   ), spawn $ XMonad.terminal myConfig)
         , ((myModMask                 , xK_p        ), spawn "synapse")
         , ((myModMask .|. shiftMask   , xK_Return   ), safeSpawn myBrowser [])
         , ((myModMask .|. controlMask , xK_Return   ), windows W.swapMaster)
         , ((myModMask .|. controlMask , xK_space    ), sendMessage ToggleLayout)
         , ((myModMask                 , xK_a        ), sendMessage MirrorExpand)
         , ((myModMask                 , xK_z        ), sendMessage MirrorShrink)
         , ((myModMask                 , xK_backslash), withFocused (sendMessage . maximizeRestore))
         , ((myModMask                 , xK_g        ), tagPrompt myXPConfig (\s -> focusUpTaggedGlobal s))
         , ((myModMask .|. controlMask , xK_g        ), tagDelPrompt myXPConfig)
         , ((myModMask .|. shiftMask   , xK_g        ), tagPrompt myXPConfig (\s -> withFocused (addTag s)))
         ]
