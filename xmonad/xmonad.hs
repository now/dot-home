import XMonad
import List
import System.Exit
import Text.Printf
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
 
import XMonad.Core
import XMonad.Util.Run
import XMonad.Prompt
import XMonad.Prompt.Input

myTerminal      = "gnome-terminal -e 'sh -c \"TERM=xterm-256color screen -d -RR\"'"
 
myBorderWidth   = 1
 
myModMask       = mod4Mask
 
myWorkspaces    = ["terminal", "web", "document" ] ++ map show [4..9]
 
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"
 
-- (top, bottom, left, right)
myDefaultGaps   = [(0,0,0,0)]
 
greenXPConfig :: XPConfig
greenXPConfig = defaultXPConfig { font        = "xft:DejaVu Sans Mono-10"
                                , bgColor     = "#181818"
                                , fgColor     = "#f6f6f6"
                                , promptBorderWidth = 0
                                , position    = Bottom
                                , height      = 16
                                , historySize = 256 }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask, xK_c), spawn $ XMonad.terminal conf)
 
    , ((modMask .|. mod1Mask, xK_c), spawn "xterm -e 'sh -c \"TERM=xterm-256color screen -d -RR\"'")

    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modMask              , xK_q     ),
          broadcastMessage ReleaseResources >> restart (Just "xmonad") True)
    , ((controlMask .|. shiftMask, xK_q     ),
          broadcastMessage ReleaseResources >> restart (Just "xmonad") True)

    , ((modMask              , xK_a     ), digraphPrompt greenXPConfig)
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
 
myLayout =
  onWorkspace "terminal" (smartBorders (Full)) $
  smartBorders $
    Mirror tiled |||
    tiled |||
    Full
  where
    tiled = Tall 2 (1/2) (3/100)
 
myManageHook = composeAll . concat $
    [ [ className =? "Firefox-bin"  --> doF (W.shift "web") ] ]

main = xmonad $ defaultConfig {
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        defaultGaps        = myDefaultGaps,
 
        keys               = myKeys,
 
        layoutHook         = myLayout,
        manageHook         = manageHook defaultConfig <+> myManageHook
    }

digraphPrompt :: XPConfig -> X ()
digraphPrompt c =
    inputPrompt c "Digraph" ?+ \digraph ->
    io $ runProcessWithInput "ratpoison-send-key" [digraph] ""
         >> return ()
