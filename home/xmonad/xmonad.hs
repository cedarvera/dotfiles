import XMonad
import XMonad.Config.Desktop
-- utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
-- layouts
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.AutoMaster
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.Gaps
-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
-- actions
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.IO

--myStartupHook :: X()
--myStartupHook = do

myTerminal :: String
myTerminal = "terminology"

myWorkspaces :: [String]
myWorkspaces = ["一" , "二" , "三" , "四" , "五" , "六" , "七" , "八" , "九" , "十"]
--, "\xf0d1"

-- colors
-- black dark/light (color0 and color8)
color00 = "#627484"
color08 = "#6986A0"
-- red dark/light (color1 and color9)
color01 = "#E49F9F"
color09 = "#E5BFBF"
-- green dark/light (color2 and color10)
color02 = "#91E380"
color10 = "#AFE0A1"
-- yellow dark/light (color3 and color11)
color03 = "#EAE47C"
color11 = "#F2FB9E"
-- blue dark/light (color4 and color12)
color12 = "#95ADD1"
-- magenta dark/light (color5 and color13)
color05 = "#DF9494"
color13 = "#F2B0B0"
-- cyan dark/light (color6 and color14)
color06 = "#8CDBD8"
color14 = "#B4F0F0"
-- white dark/light (color7 and color15)
color07 = "#E5E7EA"
color15 = "#8CDBD8"

-- Spit empty string
outEmpty :: String -> String
outEmpty str = ""

logbar h = do
  dynamicLogWithPP $ tryPP h
tryPP :: Handle -> PP
tryPP h = defaultPP
        { ppOutput          = hPutStrLn h
        , ppCurrent         = dzenColor (color08) (color14) . pad
        , ppVisible         = dzenColor (color07) (color08) . pad
        , ppHidden          = dzenColor (color07) (color08) . pad
        , ppHiddenNoWindows = dzenColor (color07) (color08) . pad
        , ppUrgent          = dzenColor (color07) (color13) . pad
        , ppOrder           = \(ws:l:t) -> [ws,l]
        , ppSep             = ""
        , ppLayout          = outEmpty
        }

myKeys conf@(XConfig { XMonad.modMask = modMask }) = M.fromList $
  [ ((modMask, xK_w), swapNextScreen)
  , ((modMask, xK_e), nextWS)
  , ((modMask, xK_r), toggleWS)
  , ((modMask, xK_b), sendMessage ToggleStruts)
  , ((modMask, xK_q), spawn "xmonad --recompile; xmonad --restart")
  -- programs
  , ((modMask .|. shiftMask, xK_f), spawn "firefox")
  ]

-- screen border padding --
padTop = 42
padBottom = 4
padRight = 4
padLeft = 4

-- layout --
layStatusGap  = gaps [(U, padTop), (R, padRight), (L, padLeft), (D, padBottom)]
layDefault    = autoMaster 1 (2/100) layAlt
layAlt        = ResizableTall 1 (2/100) (1/2) []
layFullscreen = noBorders (fullscreenFull Full)
laySpacing    = spacing 2

myLayoutHook = (layStatusGap $ avoidStruts $ laySpacing $ layDefault)
           ||| (layStatusGap $ avoidStruts $ laySpacing $ layAlt)
           ||| (layStatusGap $ avoidStruts $ laySpacing $ Accordion)
           ||| (layStatusGap $ avoidStruts $ laySpacing $ Grid)
           ||| layFullscreen

-- event --
myEventHook = ewmhDesktopsEventHook

-- manage window --
myManageHook = composeAll
  [ className =? "stalonetray" --> doIgnore
  , className =? "conky"       --> doIgnore
  ]

main = do
  leftBar <- spawnPipe "dzen2 -ta l -p -w 276 -y 4 -x 8 -h 24 -fn 'Noto Sans CJK TC' -e ''"
  -- leftBarN <- spawnPipe "dzen2 -ta l -p -w 300 -y 10 -x 278 -h 24 -fn 'FontAwesome' -e ''"
  xmonad $ ewmh defaultConfig
    { terminal           = myTerminal
    , workspaces         = myWorkspaces
    , keys               = \c -> myKeys c `M.union` keys defaultConfig c
    , modMask            = mod4Mask
    , focusFollowsMouse  = False
    , borderWidth        = 2
    , normalBorderColor  = color08
    , focusedBorderColor = color01
    , layoutHook         = windowArrange myLayoutHook
    , handleEventHook    = myEventHook
    , manageHook         = manageDocks <+> myManageHook
    , logHook            = logbar leftBar -- <+> leftBarN
    }
