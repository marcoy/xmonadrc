import System.Taffybar.Hooks.PagerHints (pagerHints)

import XMonad

import XMonad.Actions.TagWindows

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)

import XMonad.Layout.Accordion
import XMonad.Layout.Maximize
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts

import XMonad.Prompt

import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig (additionalKeysP)

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
main = do
        spawn "/usr/bin/taffybar"
        xmonad conf { startupHook = startupHook myConfig >> setWMName "LG3D" }
        -- xmonad =<< statusBar barCmd pp toggleStrutsKey conf
       where
        barCmd = "/usr/bin/xmobar /home/marcoy/.xmonad/xmobarrc"
        pp     = myPP
        conf   = myConfig
        toggleStrutsKey = toggleStrutsKey'

myConfig = ewmh $ pagerHints $ defaultConfig {
                                borderWidth = 1
                              , modMask     = myModMask
                              , terminal    = myTerminal
                              , manageHook  = manageDocks <+> myManageHook <+> manageHook defaultConfig
                              , layoutHook  = avoidStruts  $  myLayout
                              , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
                              , focusedBorderColor = "darkorange"
                              } `additionalKeysP` myKeys

-- PrettyPrinting for xmobar
myPP = defaultPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
                 , ppHidden = xmobarColor "#C98F0A" ""
                 , ppHiddenNoWindows = xmobarColor "#C9A34E" ""
                 , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                 , ppLayout = xmobarColor "#C9A34E" ""
                 , ppTitle =  xmobarColor "#C9A34E" "" . shorten 55
                 , ppSep = xmobarColor "#429942" "" " | "
                 }

-- XMonad Prompt
myXPConfig = defaultXPConfig { position = Top }

-- Layout
-- Use `onWorkspaces' to configure layout on different workspaces.
myLayout = toggleLayouts Full (maximize tiled) ||| Mirror tiled ||| tabbed shrinkText defaultTheme ||| Accordion
    where
        tiled = ResizableTall 1 (3/100) (1/2) []

-- Keys
toggleStrutsKey' XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myKeys = [ ("M-<Space>"             , sendMessage NextLayout)
         , ("M-<Return>"            , spawn $ XMonad.terminal myConfig)
         , ("M-p"                   , spawn "synapse")
         , ("M-S-<Return>"          , safeSpawn myBrowser [])
         , ("M-C-<Return>"          , windows W.swapMaster)
         , ("M-C-<Space>"           , sendMessage ToggleLayout)
         , ("M-a"                   , sendMessage MirrorExpand)
         , ("M-z"                   , sendMessage MirrorShrink)
         , ("M-\\"                  , withFocused (sendMessage . maximizeRestore))
         , ("M-g"                   , tagPrompt myXPConfig (\s -> focusUpTaggedGlobal s))
         , ("M-C-g"                 , tagDelPrompt myXPConfig)
         , ("M-S-g"                 , tagPrompt myXPConfig (\s -> withFocused (addTag s)))
         , ("M-i"                   , spawn "xcalib -invert -alter")
         , ("<XF86AudioLowerVolume>", spawn "amixer set Master 1-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 1+")
         ]
