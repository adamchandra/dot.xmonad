import XMonad
import XMonad.Util.EZConfig
import Data.Ratio
import System.Exit
import System.IO (hPutStrLn)
import XMonad.Actions.CopyWindow
import XMonad.Actions.UpdatePointer
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.Circle
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle           -- apply layout modifiers dynamically
import XMonad.Layout.MultiToggle.Instances -- ability to magnify the focused window
import XMonad.Layout.Named                 -- rename some layouts
import XMonad.Layout.NoBorders             -- get rid of borders sometimes
import XMonad.Layout.Reflect               -- ability to reflect layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation 
import XMonad.Prompt
import XMonad.Prompt.Ssh
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.CustomKeys as C

myNormalBorderColor = "#1700ff"
myFocusedBorderColor = "#ffee30"
myTerminal = "konsole"

-- (tiled ||| Mirror tiled ||| Circle ||| magnify Grid ||| Full)
myLayout = avoidStruts $
           layoutHints $
           mkToggle1 NBFULL $
           mkToggle1 REFLECTX $                                
           mkToggle1 REFLECTY $                               
           mkToggle1 NOBORDERS $
           mkToggle1 MIRROR $
           smartBorders (Circle ||| tiled ||| Grid)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask   , xK_space ), sendMessage NextLayout)
    , ((modm                 , xK_space ), sendMessage $ XMonad.Layout.MultiToggle.Toggle NBFULL)
    , ((modm .|. shiftMask   , xK_b     ), sendMessage $ XMonad.Layout.MultiToggle.Toggle NOBORDERS)           
    , ((modm .|. shiftMask   , xK_m     ), sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
    , ((modm .|. shiftMask   , xK_x     ), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX)
    , ((modm .|. shiftMask   , xK_y     ), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY)
    -- , ((modm                 , xK_q     ), restart "xmonad" True) -- %! Restart xmonad
    , ((modm .|. shiftMask   , xK_p     ), restart "xmonad" True) -- %! Restart xmonad
    , ((modm                 , xK_q     ), sendMessage $ XMonad.Layout.MultiToggle.Toggle NBFULL)
    ]

main = do
  xmonad $ gnomeConfig
             { borderWidth        = 3
             , modMask            = mod4Mask
             , manageHook         = manageHook gnomeConfig <+> myManageHook
             , focusFollowsMouse  = True
             , normalBorderColor  = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor
             , layoutHook         = myLayout
             , terminal           = myTerminal
             , keys               = \c -> myKeys c `M.union` keys gnomeConfig c
             }
   where
     myManageHook = composeAll (
      [ manageHook gnomeConfig
      , className =? "Unity-2d-panel" --> doIgnore
      , className =? "Unity-2d-launcher" --> doFloat
      ])

--     myManageHook = composeAll . concat $
--       [ [ className   =? c --> doFloat           | c <- floatByWM_CLASS]
--       , [ title       =? t --> doFloat           | t <- floatByWM_NAME]
--       , [ className   =? c --> doF (W.shift "1") | c <- toDesktop1]
--       ]
--     floatByWM_CLASS = ["Kicker", "Dialog"]
--     floatByWM_NAME = ["Calendar"]
--     toDesktop1      = [] -- example: open on desktop 1

