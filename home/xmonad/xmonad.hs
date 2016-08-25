import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Xfce
import XMonad.Actions.CycleWS
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.Circle
import XMonad.Hooks.ManageDocks

import qualified XMonad.StackSet as W
import qualified Data.Map as M

main = do
  xmonad $ xfceConfig
    { terminal           = "urxvt"
    , workspaces         = ["1", "2", "3", "4", "5"]
    , keys               = \c -> myKeys c `M.union` keys xfceConfig c
    , focusFollowsMouse  = False
    , borderWidth        = 1
    , normalBorderColor  = "#0A0A0A"
    , focusedBorderColor = "#112233"
    , layoutHook         = desktopLayoutModifiers $ Accordion ||| Grid ||| layoutHook xfceConfig
    }

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, xK_w), swapNextScreen)
  , ((modMask, xK_e), nextWS)
  , ((modMask, xK_r), toggleWS)
  , ((modMask, xK_b), sendMessage ToggleStruts)
  ]

