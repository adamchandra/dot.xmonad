{-# LANGUAGE NoMonomorphismRestriction #-}

import XMonad
import XMonad.Util.EZConfig
import Data.Ratio
import System.Exit
import System.IO (hPutStrLn)
import XMonad.Actions.CopyWindow
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FloatKeys    -- (22f
import XMonad.Actions.TopicSpace   -- (22b) set a "topic" for each workspace


-- Prompts ---------------------------------------------------
 
import XMonad.Prompt                -- (23) general prompt stuff.
import XMonad.Prompt.Man            -- (24) man page prompt
import XMonad.Prompt.AppendFile     -- (25) append stuff to my NOTES file
import XMonad.Prompt.Ssh            -- (26) ssh prompt
import XMonad.Prompt.Input          -- (26) generic input prompt, used for
                                    --      making more generic search
                                    --      prompts than those in
                                    --      XMonad.Prompt.Search
import XMonad.Prompt.Workspace      -- (27) prompt for a workspace


import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
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
import XMonad.Hooks.SetWMName
import XMonad.Actions.Navigation2D
import XMonad.Actions.WindowGo
import XMonad.Actions.WindowBringer

import qualified XMonad.Actions.FlexibleManipulate as Flex

import XMonad.Actions.GridSelect

myNormalBorderColor = "#1700ff"
myFocusedBorderColor = "#FFee20"
-- myTerminal = "urxvt"
-- myTerminal = "konsole"
myTerminal = "urxvtcd"
myShell = "zsh"


-- (tiled ||| Mirror tiled ||| Circle ||| magnify Grid ||| Full)
myLayout = 
           -- make manual gap adjustment possible.
           --gaps (zip [U,D,L,R] (repeat 0)) $
           avoidStruts $
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


-- The list of all topics/workspaces of your xmonad configuration.
 -- The order is important, new topics must be inserted
 -- at the end of the list if you want hot-restarting
 -- to work.
myTopics :: [Topic]
myTopics =
  [ "0", "1", "2", "3", "4", "5"
                             -- , "dashboard" -- the first one
  ]


myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs = M.fromList $
      [ ("dashboard", "/home/saunders")
      , ("xmonad", "/home/saunders/.xmonad")
      ]
  , defaultTopicAction = const $ spawnShell >*> 3
  , defaultTopic = "dashboard"
  , topicActions = M.fromList $
      [ ("xmonad",     spawnShellIn ".xmonad")
      , ("dashboard",  spawnShell)
      ]
  }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "sh -c '(cd ''" ++ dir ++ "'' && " ++ myShell ++ " )'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift


-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig                                    -- (23)
    { font              = "-mono-*-*-*-*-12-*-*-*-*-*-*-*"
    , bgColor           = "#333333"
    , fgColor           = "#FFFFFF"
    , fgHLight          = "#000000"
    , bgHLight          = "#BBBBBB"
    , borderColor       = "#FFFFFF"
    , promptBorderWidth = 2
    , position          = Top
    , height            = 30
    , historySize       = 256
    , historyFilter     = id
    , defaultText       = []
    , autoComplete      = Nothing
    , showCompletionOnTab = False
    }

--myXPConfig = defaultXPConfig                                    -- (23)
--    { fgColor = "#a8a3f7"
--    , bgColor = "#3f3c6d"
--    }

myGSConfig = defaultGSConfig 
    { gs_cellheight = 30
    , gs_cellwidth = 240 
    , gs_navigate = myNavigation
    }

myGSConfig2 = defaultGSConfig
   { gs_cellheight = 30
   , gs_cellwidth = 100
   , gs_navigate = myNavigation
   }


myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch myNavigation)
         ,((0,xK_Left)  , move (-1,0)  >> myNavigation)
         ,((0,xK_h)     , move (-1,0)  >> myNavigation)
         ,((0,xK_Right) , move (1,0)   >> myNavigation)
         ,((0,xK_l)     , move (1,0)   >> myNavigation)
         ,((0,xK_Down)  , move (0,1)   >> myNavigation)
         ,((0,xK_j)     , move (0,1)   >> myNavigation)
         ,((0,xK_Up)    , move (0,-1)  >> myNavigation)
         ,((0,xK_k)     , move (0,-1)  >> myNavigation)
         ,((0,xK_y)     , move (-1,-1) >> myNavigation)
         ,((0,xK_i)     , move (1,-1)  >> myNavigation)
         ,((0,xK_n)     , move (-1,1)  >> myNavigation)
         ,((0,xK_m)     , move (1,-1)  >> myNavigation)
         ,((0,xK_space) , setPos (0,0) >> myNavigation)
         ]
       -- The navigation handler ignores unknown key symbols
       navDefaultHandler = const myNavigation

------------------------------------------------------------------------
-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask   , xK_space ), sendMessage NextLayout)
    , ((modm                 , xK_space ), sendMessage $ XMonad.Layout.MultiToggle.Toggle NBFULL)
    , ((modm .|. controlMask , xK_F12   ), restart "xmonad" True) -- %! Restart xmonad
    , ((modm                 , xK_F1    ), spawn "gmrun")
    , ((modm                 , xK_F8    ), spawn "anamnesis --browse")
    , ((modm                 , xK_F2    ), spawn "disper -d auto -e -t top")
    -- , ((modm .|. controlMask   , xK_u     ), runOrRaise "emacs"  (className =? "emacs"))
    , ((modm .|. controlMask   , xK_j     ), gotoMenu)
    , ((modm .|. controlMask   , xK_k     ), bringMenu)

    -- , ((modm                 , xK_a     ), currentTopicAction myTopicConfig)
    -- , ((modm                 , xK_g     ), promptedGoto)
    -- , ((modm .|. shiftMask   , xK_g     ), promptedShift)

    --------------
    -- Navigation2d  
    , ((modm,                 xK_9    ), switchLayer)

    -- Directional navigation of windows
    , ((modm,                 xK_l), windowGo R True)  -- True==Wrap
    , ((modm,                 xK_h), windowGo L True)
    , ((modm,                 xK_k), windowGo U True)
    , ((modm,                 xK_j), windowGo D True)

    -- Swap adjacent windows
    , ((modm,                 xK_o), windowSwap R False)
    , ((modm,                 xK_y), windowSwap L False)
    , ((modm,                 xK_i), windowSwap U False)
    , ((modm,                 xK_u), windowSwap D False)

    -- Swap workspaces on adjacent screens
    , ((modm .|. controlMask, xK_l), screenSwap R False)
    , ((modm .|. controlMask, xK_h), screenSwap L False)
    , ((modm .|. controlMask, xK_k), screenSwap U False)
    , ((modm .|. controlMask, xK_j), screenSwap D False)

    -- Directional navigation of screens
    , ((modm .|. shiftMask,  xK_l), screenGo R False)
    , ((modm .|. shiftMask,  xK_h), screenGo L False)
    , ((modm .|. shiftMask,  xK_k), screenGo U False)
    , ((modm .|. shiftMask,  xK_j), screenGo D False)

    -- Send window to adjacent screen
    , ((modm .|. mod1Mask,    xK_r    ), windowToScreen R False)
    , ((modm .|. mod1Mask,    xK_l    ), windowToScreen L False)
    , ((modm .|. mod1Mask,    xK_u    ), windowToScreen U False)
    , ((modm .|. mod1Mask,    xK_d    ), windowToScreen D False)      
    -- End Navigation2d  

    -- GridSelect
    , ((modm,                 xK_8    ), goToSelected myGSConfig)
    ]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [((modm, button1), (\w -> focus w >> Flex.mouseWindow Flex.discrete w))
    ]

myNavigation2DConfig = defaultNavigation2DConfig
    { defaultTiledNavigation = centerNavigation
    , layoutNavigation = [("Circle", centerNavigation)]
    }

main = do
  xmonad $ withNavigation2DConfig myNavigation2DConfig $ gnomeConfig
             { borderWidth        = 3
             , modMask            = mod4Mask
             , manageHook         = manageHook gnomeConfig <+> myManageHook
             , focusFollowsMouse  = True
             , normalBorderColor  = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor
             , layoutHook         = myLayout
             , terminal           = myTerminal
             , keys               = \c -> myKeys c `M.union` keys gnomeConfig c
             , mouseBindings      = myMouseBindings
             , startupHook        = setWMName "LG3D"
             , workspaces         = myTopics
             }
   where
     myManageHook = composeAll (
      [ manageHook gnomeConfig
      , className =? "Unity-2d-panel" --> doIgnore
      , className =? "Unity-2d-launcher" --> doFloat
      , className =? "Gnome-panel" --> doFloat
      , className =? "Anamnesis" --> doFloat
      , className =? "Notification-daemon" --> doCenterFloat
      ])

