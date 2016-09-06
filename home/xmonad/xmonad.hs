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
myWorkspaces = [ "1", "2", "3", "4", "5" ]

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
  xmonad $ ewmh defaultConfig
    { terminal           = myTerminal
    , workspaces         = myWorkspaces
    , keys               = \c -> myKeys c `M.union` keys defaultConfig c
    , modMask            = mod4Mask
    , focusFollowsMouse  = False
    , borderWidth        = 2
    , normalBorderColor  = "#0A0A0A"
    , focusedBorderColor = "#112233"
    , layoutHook         = windowArrange myLayoutHook
    , handleEventHook    = myEventHook
    , manageHook         = manageDocks <+> myManageHook
    }
