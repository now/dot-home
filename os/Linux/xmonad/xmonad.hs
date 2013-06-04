import XMonad
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Layout
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

import XMonad.Core
import XMonad.Util.Run
import XMonad.Prompt
import XMonad.Prompt.Input

myTerminal = "gnome-terminal -e 'sh -c \"TERM=xterm-256color screen -d -RR\"'"

myModMask = mod4Mask

myWorkspaces = ["terminal", "web", "document" ] ++ map show [4..9]

myXPConfig = defaultXPConfig { font = "xft:DejaVu Sans Mono-10"
                             , bgColor = "#181818"
                             , fgColor = "#f6f6f6"
                             , promptBorderWidth = 0
                             , position = Bottom
                             , height = 16
                             , historySize = 256 }

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                   xK_c), spawn $ XMonad.terminal conf)
    , ((modMask .|. mod1Mask,      xK_c), spawn "xterm -e 'sh -c \"TERM=xterm-256color screen -d -RR\"'")
    , ((modMask,                   xK_w), kill)
    , ((modMask,                   xK_r), sendMessage NextLayout)
    , ((modMask .|. shiftMask,     xK_r), setLayout $ XMonad.layoutHook conf)
    , ((modMask,                   xK_l), refresh)
    , ((modMask,                   xK_a), digraphPrompt myXPConfig)
    ]

myLayout =
  onWorkspace "terminal" (gaps [(L, 177), (R, 177), (U, 0), (D, 0)] $ smartBorders (Full)) $
  smartBorders $
    tiled |||
    Mirror tiled |||
    Full
  where
    tiled = Tall 1 (3/100) (1/2)

myManageHook = composeAll . concat $
    [ [ className =? "Firefox"  --> doF (W.shift "web") ] ]

main = xmonad $ defaultConfig
    { terminal = myTerminal
    , modMask = myModMask
    , workspaces = myWorkspaces
    , keys = \c -> myKeys c `M.union` keys defaultConfig c
    , layoutHook = myLayout
    , manageHook = manageHook defaultConfig <+> myManageHook
    }

digraphPrompt c =
    inputPrompt c "Digraph" ?+ \digraph ->
    io $ runProcessWithInput "xdigraph" [digraph] ""
         >> return ()
