  import XMonad
import XMonad.Layout
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import System.IO (hPutStrLn)
import XMonad.Prompt
import XMonad.Prompt.Ssh
import qualified Data.Map as M
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import Data.Ratio
import XMonad.Layout.LayoutHints
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CopyWindow
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
 
 
myNormalBorderColor = "#000000"
myFocusedBorderColor = "#eef204"
 
myManageHook = composeAll [ className =? "XCalc" --> doFloat
                          , className =? "display" --> doFloat ]
 
newManageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks
 
myKeys x =      
    [((m .|. modMask x, k), windows $ f i)
         | (i, k) <- zip (workspaces x) [xK_1 ..]      
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
    ++
    [ ((modMask x .|. controlMask, xK_s), sshPrompt defaultXPConfig) 
    , ((modMask x .|. shiftMask, xK_f), spawn "mpc toggle") -- toggle mpc playing or not
    , ((modMask x              , xK_v), spawn "mpc volume +10") -- raise volume
    , ((modMask x .|. shiftMask, xK_v), spawn "mpc volume -10") -- lower volume
    , ((modMask x .|. controlMask .|. shiftMask, xK_u), spawn "sudo s2disk") -- suspend to disk
    ]
 
newKeys x = M.union (keys defaultConfig x) (M.fromList (myKeys x))
 
 
myLayoutHook = avoidStruts (layoutHints (tiled ||| Mirror tiled ||| Circle ||| magnify Grid ||| Full))
 
    where 
      tiled = Tall nmaster delta ratio
      nmaster = 1
      delta = 3/100
      ratio = 1/2
      magnify = magnifiercz (12%10)
 
 
main = do
 
  xmobar <- spawnPipe "xmobar"
  xmonad $ defaultConfig
             { borderWidth = 2
             , modMask = mod3Mask 
             , manageHook = newManageHook 
             , logHook = dynamicLogWithPP defaultPP { ppOutput = hPutStrLn xmobar} >> updatePointer (Relative 1 1)
             , keys = newKeys
             , layoutHook = myLayoutHook
             , normalBorderColor = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor
             }



    ++
    -- move floating windows with keybindings                   -- (22f)
    [ ("M-a M-<" ++ dir ++ ">", withFocused (keysMoveWindow (dx,dy)))
      | (dir,dx,dy) <- [ ("L", -20, 0)
                       , ("R", 20, 0)
                       , ("U", 0, -20)
                       , ("D", 0, 20) ]
    ]


































 -- The list of all topics/workspaces of your xmonad configuration.
 -- The order is important, new topics must be inserted
 -- at the end of the list if you want hot-restarting
 -- to work.
 myTopics :: [Topic]
 myTopics =
   [ "dashboard" -- the first one
   , "admin", "build", "cleaning", "conf", "darcs", "haskell", "irc"
   ]

 myTopicConfig :: TopicConfig
 myTopicConfig = defaultTopicConfig
   { topicDirs = M.fromList $
       [ ("conf", "w/conf")
       , ("dashboard", "Desktop")
       , ("yi", "w/dev-haskell/yi")
       , ("darcs", "w/dev-haskell/darcs")
       , ("haskell", "w/dev-haskell")
       , ("xmonad", "w/dev-haskell/xmonad")
       , ("tools", "w/tools")
       , ("movie", "Movies")
       , ("talk", "w/talks")
       , ("music", "Music")
       , ("documents", "w/documents")
       , ("pdf", "w/documents")
       ]
   , defaultTopicAction = const $ spawnShell >*> 3
   , defaultTopic = "dashboard"
   , topicActions = M.fromList $
       [ ("conf",       spawnShell >> spawnShellIn "wd/ertai/private")
       , ("darcs",      spawnShell >*> 3)
       , ("yi",         spawnShell >*> 3)
       , ("haskell",    spawnShell >*> 2 >>
                        spawnShellIn "wd/dev-haskell/ghc")
       , ("xmonad",     spawnShellIn "wd/x11-wm/xmonad" >>
                        spawnShellIn "wd/x11-wm/xmonad/contrib" >>
                        spawnShellIn "wd/x11-wm/xmonad/utils" >>
                        spawnShellIn ".xmonad" >>
                        spawnShellIn ".xmonad")
       , ("mail",       mailAction)
       , ("irc",        ssh somewhere)
       , ("admin",      ssh somewhere >>
                        ssh nowhere)
       , ("dashboard",  spawnShell)
       , ("twitter",    spawnShell)
       , ("web",        spawn browserCmd)
       , ("movie",      spawnShell)
       , ("documents",  spawnShell >*> 2 >>
                        spawnShellIn "Documents" >*> 2)
       , ("pdf",        spawn pdfViewerCmd)
       ]
   }

 -- extend your keybindings
 myKeys conf@XConfig{modMask=modm} =
   [ ((modm              , xK_n     ), spawnShell) -- %! Launch terminal
   , ((modm              , xK_a     ), currentTopicAction myTopicConfig)
   , ((modm              , xK_g     ), promptedGoto)
   , ((modm .|. shiftMask, xK_g     ), promptedShift)
   {- more  keys ... -}
   ]
   ++
   [ ((modm, k), switchNthLastFocused myTopicConfig i)
   | (i, k) <- zip [1..] workspaceKeys]

 spawnShell :: X ()
 spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

 spawnShellIn :: Dir -> X ()
 spawnShellIn dir = spawn $ "urxvt '(cd ''" ++ dir ++ "'' && " ++ myShell ++ " )'"

 goto :: Topic -> X ()
 goto = switchTopic myTopicConfig

 promptedGoto :: X ()
 promptedGoto = workspacePrompt myXPConfig goto

 promptedShift :: X ()
 promptedShift = workspacePrompt myXPConfig $ windows . W.shift

 myConfig = do
     checkTopicConfig myTopics myTopicConfig
     myLogHook <- makeMyLogHook
     return $ defaultConfig
          { borderWidth = 1 -- Width of the window border in pixels.
          , workspaces = myTopics
          , layoutHook = myModifiers myLayout
          , manageHook = myManageHook
          , logHook = myLogHook
          , handleEventHook = myHandleEventHook
          , terminal = myTerminal -- The preferred terminal program.
          , normalBorderColor = "#3f3c6d"
          , focusedBorderColor = "#4f66ff"
          , XMonad.modMask = mod1Mask
          , keys = myKeys
          , mouseBindings = myMouseBindings
          }

 main :: IO ()
 main = xmonad =<< myConfig
