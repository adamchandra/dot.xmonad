import XMonad                          -- (0) core xmonad libraries
 
import qualified XMonad.StackSet as W  -- (0a) window stack manipulation
import qualified Data.Map as M         -- (0b) map creation
import Data.List ((\\), find)
import Data.Maybe (isJust, catMaybes)
import Data.Monoid
 
import System.Posix.Unistd
import Control.Concurrent (threadDelay)
 
import XMonad.Layout.MagicFocus    -- (0c)
 
-- Hooks -----------------------------------------------------
 
import XMonad.Hooks.DynamicLog     -- (1)  for dzen status bar
  hiding (pprWindowSet)
import XMonad.Hooks.UrgencyHook    -- (2)  alert me when people use my nick
                                   --      on IRC
import XMonad.Hooks.ManageDocks    -- (3)  automatically avoid covering my
                                   --      status bar with windows
import XMonad.Hooks.ManageHelpers  -- (4)  for doCenterFloat, put floating
                                   --      windows in the middle of the
                                   --      screen
 
-- Layout ----------------------------------------------------
 
import XMonad.Layout.ResizableTile -- (5)  resize non-master windows too
import XMonad.Layout.Grid          -- (6)  grid layout
import XMonad.Layout.TwoPane
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders     -- (7)  get rid of borders sometimes
                                   -- (8)  navigate between windows
import XMonad.Layout.WindowNavigation  --  directionally
import XMonad.Layout.Named         -- (9)  rename some layouts
import XMonad.Layout.PerWorkspace  -- (10) use different layouts on different WSs
import XMonad.Layout.WorkspaceDir  -- (11) set working directory
                                   --      per-workspace
import XMonad.Layout.Reflect       -- (13) ability to reflect layouts
import XMonad.Layout.MultiToggle   -- (14) apply layout modifiers dynamically
import XMonad.Layout.MultiToggle.Instances
 
                                   -- (15) ability to magnify the focused
                                   --      window
import qualified XMonad.Layout.Magnifier as Mag
 
import XMonad.Layout.Gaps          -- (15a) add manual gaps around the sides
                                   --       of the screen useful for things
                                   --       like screencasts or projectors
                                   --       which cut off part of the screen
 
import XMonad.Layout.Combo         -- (15b) combine layouts
 
-- Actions ---------------------------------------------------
 
import XMonad.Actions.CycleWS      -- (16) general workspace-switching
                                   --      goodness
import XMonad.Actions.CycleRecentWS -- (17) cycle between workspaces
                                    --      in most-recently-used order
import XMonad.Actions.Warp         -- (18) warp the mouse pointer
import XMonad.Actions.Submap       -- (19) create keybinding submaps
import XMonad.Actions.Search hiding (Query, images)
                                   -- (20) some predefined web searches
-- import XMonad.Actions.WindowGo  -- (21) runOrRaise
import XMonad.Actions.WithAll      -- (22) do something with all windows on a workspace
import XMonad.Actions.SpawnOn      -- (22a) start programs on a particular WS
 
import XMonad.Actions.TopicSpace   -- (22b) set a "topic" for each workspace
import XMonad.Actions.DynamicWorkspaces  
                                   -- (22c)
 
import XMonad.Actions.DynamicWorkspaceGroups 
                                   -- (22d)
 
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO 
                                   -- (22e)
 
import XMonad.Actions.FloatKeys    -- (22f)
 
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
 
-- Utilities -------------------------------------------------
 
import XMonad.Util.Loggers          -- (28) some extra loggers for my
                                    --      status bar
import XMonad.Util.EZConfig         -- (29) "M-C-x" style keybindings
import XMonad.Util.NamedScratchpad  -- (30) 'scratchpad' terminal
import XMonad.Util.Run              -- (31) for 'spawnPipe', 'hPutStrLn'
 
import Control.Monad (when)
                                                                -- (31)
main :: IO ()
main = do h <- spawnPipe "dzen2 -ta r -fg '#a8a3f7' -bg '#3f3c6d' -e 'onstart=lower'"
          host <- getHost
          checkTopicConfig (myTopicNames host) (myTopicConfig host) -- (22b)
          xmonad $ byorgeyConfig h host                         -- (0)
 
data Host = Desktop | Laptop Bool -- ^ Does the laptop have a Windows key?
  deriving (Eq, Read, Show)
 
getHost :: IO Host
getHost = do
  hostName <- nodeName `fmap` getSystemID
  return $ case hostName of
    "archimedes" -> Laptop True
    "euclid"     -> Laptop False
    "LVN513-12"  -> Desktop
    _            -> Desktop
 
myTerminal = "urxvt --perl-lib ~/.urxvt -fg lightgrey -bg black +sb"
myShell = "zsh"
 
byorgeyConfig h host = myUrgencyHook $                         -- (2)
     defaultConfig
       {
         borderWidth        = 2
       , terminal           = myTerminal
       , workspaces         = myTopicNames host
       , modMask            = if host == Laptop False
                                then modMask defaultConfig
                                else mod4Mask
 
       , normalBorderColor  = "#dddddd"
       , focusedBorderColor = "#0033ff"
                                                                -- (22)
       , logHook            = myDynamicLog h host
       , manageHook         = manageSpawn
                              <+> myManageHook
                              <+> manageHook defaultConfig
       , layoutHook         = myLayoutHook
       , focusFollowsMouse  = False
 
         -- XXX fixme: comment!                                 -- (29)
       , startupHook        = return () >>
                              checkKeymap (byorgeyConfig h host)
                                          (myKeys h host)
 
                                              -- (0c), and see below
       , handleEventHook    = followOnlyIf (queryFocused whenToFollow)
       }
       `additionalKeysP` (myKeys h host)                        -- (29)
 
checkWS :: (String -> Bool) -> String -> X ()
checkWS p w = do cw <- gets (W.currentTag . windowset)
                 when (not $ p cw) $ (windows $ W.greedyView w)
 
-- have urgent events flash a yellow dzen bar with black text
myUrgencyHook = withUrgencyHook dzenUrgencyHook                 -- (2)
    { args = ["-bg", "yellow", "-fg", "black"] }
 
data TopicItem = TI { topicName :: Topic   -- (22b)
                    , topicDir  :: Dir
                    , topicAction :: X ()
                    }
 
-- define some custom topics for use with the TopicSpace module.
myTopics :: Host -> [TopicItem]
myTopics host =
  [ TI "web" "" (spawn "firefox")
  , TI "irc" "" (ircAction host)
  , TI "mail" "" (runInTerm "" "ssh en")
  , ti "read" "papers"
  , ti "write" "writing/blog/stream-comonad"
  , TI "org" "notes"
    (spawn "emacs --name org ~/notes/journal.org")
  , TI "draw" "" (spawn "inkscape")
  , TI "xm-conf" ".xmonad"
    (edit "~/.xmonad/xmonad.hs" >>
     shell)
  , ti "xm-hack" "src/xmonad/XMonadContrib"
  , TI "em-conf" "" (edit "~/.emacs")
  , TI "music" "music" (spawn "rhythmbox")
  , TI "net" "" (spawn "wicd-client -n" >>
                 shell)
  , ti "conf" ""
  , ti "misc" ""
  , ti "500" "teaching/500/sf"
  , ti "ref" "documents/reference"
  , ti "play" ""
  , TI "tex-conf" "texmf/tex" (edit "~/texmf/tex/brent.sty")
  , ti "mlt" "writing/mlt"
  , ti "MR" "writing/Monad.Reader/issues/Issue19"
  , ti "mc" "teaching/mathcounts"
  , ti "dia" "src/diagrams"
  , ti "dia-doc" "src/diagrams/doc"
  , ti "dia-core" "src/diagrams/core"
  , ti "dia-lib" "src/diagrams/lib"
  , ti "dia-cairo" "src/diagrams/cairo"
  , ti "dia-contrib" "src/diagrams/contrib"
  , ti "sp" "research/species/nsf11"
  , ti "fc"  "src/gparam/fc"
  , ti "geb" "teaching/geb"
  , ti "pweb" "documents/sites/upenn"
  , ti "hask" "teaching/haskell"
  , ti "anki" "local/lib/anki-1.2.8"
  , ti "ghc" "src/ghc-new-tc"
  , ti "CG" "documents/CG"
  , ti "replib" "src/replib"
  , ti "unbound" "src/replib/Unbound"
  , TI "video" "video" (spawn "cinelerra")
  , TI "aop" "learning/aop" (spawnShell host
                             >> spawn "emacs ~/learning/aop/aop.lhs")
  , ti "tc" "writing/typeclassopedia"
  , ti "noah" "documents/noah/schedule"
  ]
  where
    -- Make a default topic item that just spawns a shell.
    ti t d = TI t d shell
    shell = spawnShell host
 
ircAction :: Host -> X ()
ircAction host = case host of
  Laptop _ -> runInTerm "" "ssh byorgey@eniac.seas.upenn.edu"
  Desktop  -> runInTerm "" "screen -dRR"
 
edit :: String -> X ()
edit = spawn . ("em "++)
 
myTopicNames :: Host -> [Topic]
myTopicNames = map topicName . myTopics
 
-- (22b)
myTopicConfig :: Host -> TopicConfig
myTopicConfig host = defaultTopicConfig
  { topicDirs = M.fromList $ map (\(TI n d _) -> (n,d)) myTopics'
  , defaultTopicAction = const (return ())
  , defaultTopic = "web"
  , maxTopicHistory = 10
  , topicActions = M.fromList $ map (\(TI n _ a) -> (n,a)) myTopics'
  }
 where myTopics' = myTopics host
 
spawnShell :: Host -> X ()
spawnShell host = currentTopicDir (myTopicConfig host) >>= spawnShellIn
 
spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerminal ++ " -title urxvt -e sh -c 'cd ''" ++ dir ++ "'' && " ++ myShell ++ "'"
 
delay :: X ()
delay = io (threadDelay 0)  -- I no longer remember what this is for
 
goto :: Host -> Topic -> X ()
goto host t = delay >> switchTopic' W.view (myTopicConfig host) t  -- (22b)
 
promptedGoto :: Host -> X ()
promptedGoto = workspacePrompt myXPConfig . goto    -- (27)
 
promptedGotoOtherScreen :: Host -> X ()
promptedGotoOtherScreen host =
  workspacePrompt myXPConfig $ \ws -> do            -- (27)
    nextScreen
    goto host ws
 
promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift  -- (27)
 
-- XXX offset scratchpad windows by a bit --- each one different?
scratchpadSize = W.RationalRect (1/4) (1/4) (1/2) (1/2)
mySPFloat = customFloating scratchpadSize
 
customTerm = "urxvt-custom"
 
scratchpads =
  [ NS "term"  (customTerm ++ " -title term") (title =? "term") mySPFloat
  , NS "term2" (customTerm ++ " -title term2") (title =? "term2") mySPFloat
  , NS "ghci"  (customTerm ++ " -e ghci") (title =? "ghci") mySPFloat
  , NS "sync"  (customTerm ++ " -e sy") (title =? "sy") mySPFloat
  , NS "top"   (customTerm ++ " -e htop") (title =? "htop") mySPFloat
  ]
 
myDynamicLog h host = dynamicLogWithPP $ byorgeyPP              -- (1)
  { ppVisible = dzenColor "blue" "#a8a3f7" . pad
  , ppExtras = [ date "%a %b %d  %I:%M %p"                      -- (1,28)
               , loadAvg                                        -- (28)
               ]
               ++ (case host of Laptop _ -> [battery]
                                _        -> [])
  , ppOrder  = \(ws:l:t:exs) -> [t,l,ws]++exs                   -- (1)
  , ppOutput = hPutStrLn h                                      -- (1,31)
  , ppTitle  = shorten (case host of Laptop _ -> 45
                                     Desktop  -> 60)
  , ppSort   = fmap (namedScratchpadFilterOutWorkspace.) DO.getSortByOrder
  , ppHiddenNoWindows = const ""
  }
 
-- my custom keybindings.
myKeys h host = myKeymap host (byorgeyConfig h host)
 
myKeymap host conf =
 
    -- mod-[1..],       Switch to workspace N
    -- mod-shift-[1..], Move client to workspace N
    -- mod-ctrl-[1..],  Switch to workspace N on other screen
    [ (m ++ "M-" ++ [k], f i)                                   -- (0)
        | (i, k) <- zip (XMonad.workspaces conf) "1234567890-=[]\\" -- (0)
        , (f, m) <- [ (goto', "")                    -- (0a)
                    , (windows . W.shift, "S-")
                    , (\ws -> nextScreen >> (goto' $ ws), "C-")
                    ]
    ]
 
    ++
    [ ("M-S-x", spawnShell host)                          -- (0)
    , ("M-S-b", spawn "urxvt-big")
    , ("M-g",   promptedGoto host)
    , ("M-C-g", promptedGotoOtherScreen host)
    , ("M-S-g", promptedShift)
    , ("M-S-C-g", workspacePrompt myXPConfig $ \ws ->          -- (27)
                    withAll' (W.shiftWin ws) >> goto host ws)  -- (22)
 
      -- in conjunction with manageHook, open a small temporary
      -- floating terminal
    , ("M-a s", namedScratchpadAction scratchpads "term")       -- (30)
    , ("M-a d", namedScratchpadAction scratchpads "term2")
    , ("M-a g", namedScratchpadAction scratchpads "ghci")
    , ("M-a t", namedScratchpadAction scratchpads "top")
    ]
    ++
 
    -- move floating windows with keybindings                   -- (22f)
    [ ("M-a M-<" ++ dir ++ ">", withFocused (keysMoveWindow (dx,dy)))
      | (dir,dx,dy) <- [ ("L", -20, 0)
                       , ("R", 20, 0)
                       , ("U", 0, -20)
                       , ("D", 0, 20) ]
    ]
 
    -- sync using Unison in a new floating window, but only on my laptop
    ++ (case host of Laptop _ ->
                       [("M-a y", namedScratchpadAction scratchpads "sync")]
                     _ -> []
       )
 
    ++
    [ ("M-S-a", kill)                                           -- (0)
    , ("M-S-C-a", killAll)                                      -- (22)
 
    -- some gap-toggling
    , ("M-C-p b", sendMessage $ ToggleStrut D)                    -- (3)
    , ("M-C-p t", sendMessage $ ToggleStrut U)                    --  "
    , ("M-C-p a", sendMessage $ ToggleStruts)                     --  "
 
    , ("M-C-p g", sendMessage $ ToggleGaps)                       -- (15a)
    ]
 
    ++
    [ ("M-C-p " ++ f ++ " <" ++ dk ++ ">", sendMessage $ m d)     -- (15a)
        | (dk, d) <- [("L",L), ("D",D), ("U",U), ("R",R)]
        , (f, m)  <- [("v", ToggleGap), ("h", IncGap 40), ("f", DecGap 10)]
    ]
 
    ++
    -- rotate workspaces.
--    [ ("M-C-<R>",   nextWS )                   -- (16)
--    , ("M-C-<L>",   prevWS )                   --  "
    [ ("M-C-<R>",   DO.swapWith Next NonEmptyWS)                -- (22e)
    , ("M-C-<L>",   DO.swapWith Prev NonEmptyWS)                -- "
    , ("M-S-<R>",   DO.shiftTo Next HiddenNonEmptyWS)           -- "
    , ("M-S-<L>",   DO.shiftTo Prev HiddenNonEmptyWS)           -- "
    , ("M-<R>",     delay >> DO.moveTo Next HiddenNonEmptyWS)   -- "
    , ("M-<L>",     delay >> DO.moveTo Prev HiddenNonEmptyWS)   -- "
    , ("M-f",       newCodeWS)                                  -- see below
 
    -- expand/shrink windows
    , ("M-r k", sendMessage MirrorExpand)                       -- (5)
    , ("M-r j", sendMessage MirrorShrink)                       -- (5)
    , ("M-r h", sendMessage Shrink)                             -- (0)
    , ("M-r l", sendMessage Expand)                             -- (0)
 
    -- switch to previous workspace
    , ("M-z", delay >> toggleWS)                                -- (16)
 
    -- cycle workspaces in most-recently-used order
    -- see definition of custom cycleRecentWS' below, and also     (17)  
    , ("M-S-<Tab>", cycleRecentWS' [xK_Super_L, xK_Shift_L] xK_Tab xK_grave)
 
    -- close all windows on current workspace and move to next  
    , ("M-S-z", killAll >> DO.moveTo Prev HiddenNonEmptyWS)     -- (22, 22e)
 
    -- dynamic workspace bindings
    , ("M-n", addWorkspacePrompt myXPConfig)                    -- (22c)
    , ("M-S-n", renameWorkspace myXPConfig)                     -- "
    , ("M-C-r", removeWorkspace)                                -- "
    , ("M-C-S-r", killAll >> removeWorkspace)                   -- 
 
    -- move between screens
    , ("M-s", nextScreen)
    , ("M-w", swapNextScreen)
    , ("M-e", shiftNextScreen)
 
      -- lock the screen with xscreensaver
    , ("M-S-l", spawn "xscreensaver-command -lock")             -- (0)
 
    -- bainsh the pointer
    , ("M-'", banishScreen LowerRight)                          -- (18)
    , ("M-b", warpToWindow (1/2) (1/2))
 
    -- some programs to start with keybindings.
    , ("M-x f", spawnOn "web" "firefox")                        -- (22a)
    , ("M-x o", spawnOn "web" "opera")                          -- "
    , ("M-x g", spawnOn "draw" "gimp")                          -- "
    , ("M-x m", spawn "rhythmbox")                              -- (0)
    , ("M-x t", spawn "xclock -update 1")                       -- (0)
    , ("M-x S-g", spawn "javaws ~/playing/go/cgoban.jnlp")      -- (0)
    , ("M-x n", goto' "org")
 
    -- configuration.
    , ("M-c x", goto' "xm-conf")
    , ("M-c e", goto' "em-conf")
    , ("M-c t", goto' "tex-conf")
    ] ++
    (case host of Laptop _ -> [("M-c n", goto' "net")]
                  _        -> [])
    ++
    [ ("M-c v", spawn "urxvt -e alsamixer")                     -- (0)
    , ("M-c k", spawn "xkill")
    , ("M-c M-S-a", killAll)
 
    -- window navigation keybindings.
    , ("C-<R>", sendMessage $ Go R)                             -- (8)
    , ("C-<L>", sendMessage $ Go L)                             --  "
    , ("C-<U>", sendMessage $ Go U)                             --  "
    , ("C-<D>", sendMessage $ Go D)                             --  "
    , ("S-C-<R>", sendMessage $ Swap R)                         --  "
    , ("S-C-<L>", sendMessage $ Swap L)                         --  "
    , ("S-C-<U>", sendMessage $ Swap U)                         --  "
    , ("S-C-<D>", sendMessage $ Swap D)                         --  "
    , ("S-M-C-<R>", sendMessage $ Move R)
    , ("S-M-C-<L>", sendMessage $ Move L)
    , ("S-M-C-<U>", sendMessage $ Move U)
    , ("S-M-C-<D>", sendMessage $ Move D)
 
    -- switch to urgent window
    , ("M-u", focusUrgent)
 
    -- toggles: fullscreen, flip x, flip y, mirror, no borders
    , ("M-C-<Space>", sendMessage $ Toggle NBFULL)              -- (14)
    , ("M-C-x",       sendMessage $ Toggle REFLECTX)            -- (14,13)
    , ("M-C-y",       sendMessage $ Toggle REFLECTY)            -- (14,13)
    , ("M-C-m",       sendMessage $ Toggle MIRROR)              --  "
    , ("M-C-b",       sendMessage $ Toggle NOBORDERS)           --  "
 
    -- some prompts.
      -- ability to change the working dir for a workspace.
    , ("M-p d", changeDir myXPConfig)                           -- (11)
      -- man page prompt
    , ("M-p m", manPrompt myXPConfig)                           -- (24)
      -- add single lines to my NOTES file from a prompt.       -- (25)
    , ("M-p n", appendFilePrompt myXPConfig "$HOME/NOTES")
      -- shell prompt.
    , ("M-p s", sshPrompt myXPConfig)                         -- (26)
    , ("M-p e", spawn "exe=`echo | yeganesh -x` && eval \"exec $exe\"") 
 
      -- some searches.
    , ("M-/", submap . mySearchMap $ myPromptSearch)            -- (19,20)
    , ("M-C-/", submap . mySearchMap $ mySelectSearch)          -- (19,20)
 
    -- some random utilities.
    , ("M-C-c", spawn "dzen-cal")  -- calendar
    , ("<Print>", spawn "scrot")
    , ("C-<Print>", spawn "sleep 0.2; scrot -s")
 
    , ("M-y n", promptWSGroupAdd myXPConfig "Name this group: ")  -- (22d)
    , ("M-y g", promptWSGroupView myXPConfig "Go to group: ")     -- (22d)
    , ("M-y d", promptWSGroupForget myXPConfig "Forget group: ")  -- (22d)
 
    -- volume control.
    , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+ unmute")
 
--    , ("<XF86Display>", spawn "sudo pm-suspend")   -- this never worked very well
    ]
  where goto' = goto host
 
-- Find the first empty workspace named "code<i>" for <i> some integer,
-- or create a new one
newCodeWS :: X ()
newCodeWS = withWindowSet $ \w -> do
  let wss = W.workspaces w
      cws = map W.tag $ filter (\ws -> "code" `isPrefixOf` W.tag ws && isJust (W.stack ws)) wss
      num = head $ [0..] \\ catMaybes (map (readMaybe . drop 4) cws)
      new = "code" ++ show num
  when (not $ new `elem` (map W.tag wss)) $ addWorkspace new
  windows $ W.view new
 where readMaybe s = case reads s of
                       [(r,_)] -> Just r
                       _       -> Nothing
 
-- modified variant of cycleRecentWS from XMonad.Actions.CycleRecentWS (17)
-- which does not include visible but non-focused workspaces in the cycle
cycleRecentWS' = cycleWindowSets options
 where options w = map (W.view `flip` w) (recentTags w)
       recentTags w = map W.tag $ W.hidden w ++ [W.workspace (W.current w)]
 
-- Perform a search, using the given method, based on a keypress
mySearchMap method = M.fromList $                               -- (0b)
        [ ((0, xK_g), method google)                            -- (20)
        , ((0, xK_w), method wikipedia)                         --  "
        , ((0, xK_h), method hoogle)                            --  "
        , ((shiftMask, xK_h), method hackage)                   --  "
        , ((0, xK_s), method scholar)                           --  "
        , ((0, xK_m), method mathworld)                         --  "
        , ((0, xK_p), method maps)                              --  "
        , ((0, xK_d), method dictionary)                        --  "
        , ((0, xK_a), method alpha)                             --  "
        , ((0, xK_l), method lucky)                             --  "
 
        -- custom searches (see below)  
        , ((0, xK_i), method images)                            
        , ((0, xK_k), method greek)
        ]
 
-- Search Perseus for ancient Greek dictionary entries
greek  = searchEngine "greek"  "http://www.perseus.tufts.edu/hopper/morph?la=greek&l="
 
-- for some strange reason the image search that comes with the Search module
-- is for google.fr
images = searchEngine "images" "http://www.google.com/search?hl=en&tbm=isch&q="
 
-- Prompt search: get input from the user via a prompt, then
--   run the search in firefox and automatically switch to the web
--   workspace
myPromptSearch (SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ \s ->                    -- (27)
      (search "firefox" site s >> viewWeb)                      -- (0,20)
 
-- Select search: do a search based on the X selection
mySelectSearch eng = selectSearch eng >> viewWeb                -- (20)
 
-- Switch to the "web" workspace
viewWeb = windows (W.view "web")                                -- (0,0a)
 
-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig                                    -- (23)
    { fgColor = "#a8a3f7"
    , bgColor = "#3f3c6d"
    }
 
-- Set up a customized manageHook (rules for handling windows on
--   creation)
myManageHook :: ManageHook                                      -- (0)
myManageHook = composeAll $
                   -- auto-float certain windows
                 [ className =? c --> doCenterFloat | c <- myFloats ] -- (4)
                 ++
                 [ fmap (t `isPrefixOf`) title --> doFloat | t <- myFloatTitles ]
                 ++
                   -- send certain windows to certain workspaces
                 [ className =? "Rhythmbox" --> doF (W.shift "music") -- (0,0a)
                   -- unmanage docks such as gnome-panel and dzen
                 , manageDocks                                     -- (3)
                   -- manage the scratchpad terminal window
                 , namedScratchpadManageHook scratchpads           -- (30)
                 , appName =? "xbuffy-main" --> doFloatAt 0.92 0.66
                 , appName =? "xbuffy-aux"  --> doFloatAt 0.92 0.81
                 , appName =? "Caml graphics" --> doFloat
                 ]
    -- windows to auto-float
    where myFloats = [ "Volume"
                     , "XClock"
                     , "Network-admin"
                     , "Xmessage"
                     , "gnome-search-tool"
                     , "Qjackctl.bin"
                     , "Icfp"
                     , "Floating"
                     , "Game"
                     , "Caml graphics"
                     ]
          myFloatTitles = ["Bridge Bid", "Pong", "Floating"]
 
-- specify a custom layout hook.
myLayoutHook =
 
    -- automatically avoid overlapping my dzen status bar.
    avoidStrutsOn [U] $                                        -- (3)
 
    -- make manual gap adjustment possible.
    gaps (zip [U,D,L,R] (repeat 0)) $
 
    -- start all workspaces in my home directory, with the ability
    -- to switch to a new working dir.                          -- (10,11)
    workspaceDir "~" $
 
    -- navigate directionally rather than with mod-j/k
    configurableNavigation (navigateColor "#00aa00") $          -- (8)
 
    -- ability to toggle between fullscreen, reflect x/y, no borders,
    -- and mirrored.
    mkToggle1 NBFULL $                                  -- (14)
    mkToggle1 REFLECTX $                                -- (14,13)
    mkToggle1 REFLECTY $                                -- (14,13)
    mkToggle1 NOBORDERS $                               --  "
    mkToggle1 MIRROR $                                  --  "
 
    -- borders automatically disappear for fullscreen windows.
    smartBorders $                                              -- (7)
 
    -- "web" and "irc" start in Full mode and can switch to tiled...
    onWorkspaces ["web","irc"] (Full ||| myTiled) $               -- (10,0)
 
    -- ...whereas all other workspaces start tall and can switch
    -- to a grid layout with the focused window magnified.
    myTiled |||           -- resizable tall layout
    Mag.magnifier Grid |||                                      -- (15,6)
    TwoPane (3/100) (1/2) |||
    (named "Full|Acc" $ combineTwo myTiled Full Accordion)      -- (15b)
 
-- use ResizableTall instead of Tall, but still call it "Tall".
myTiled = named "Tall" $ ResizableTall 1 0.03 0.5 []            -- (9,5)
 
 
findTag p = find p . map W.tag . W.workspaces
 
selectWorkspace' :: XPConfig -> X ()
selectWorkspace' conf = workspacePrompt conf $ \w ->
                        do s <- gets windowset
                           case findTag (w `isPrefixOf`) s of
                             Just w' -> windows $ W.greedyView w'
                             Nothing -> return ()
 
-- Improved version of followOnlyIf from MagicFocus
followOnlyIfQ :: Query Bool -> Event -> X All
followOnlyIfQ cond e@(CrossingEvent {ev_window = w, ev_event_type = t})
    | t == enterNotify && ev_mode e == notifyNormal
    = whenX (runQuery cond w) (focus w) >> return (All False)
followOnlyIfQ _ _ = return $ All True
 
-- Focus follows mouse only for Gimp windows
whenToFollow :: Query Bool
whenToFollow = (className =? "Gimp")
 
queryFocused :: Query Bool -> X Bool
queryFocused q = withWindowSet $ maybe (return False) (runQuery q) . W.peek

