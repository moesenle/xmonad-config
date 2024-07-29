
import Control.Monad               (void, when)
import Data.Default                (def)
import System.Directory            (doesFileExist, getHomeDirectory)
import System.Exit                 (exitSuccess)
import System.FilePath.Posix       ((</>))
import System.IO                   (Handle, hPutStrLn)
import System.Posix.Process        (executeFile)
import XMonad
import XMonad.Actions.CycleWS      (Direction1D (..), WSType (..), moveTo, nextWS, prevWS, shiftNextScreen, shiftToNext,
                                    shiftToPrev, swapNextScreen)
import XMonad.Actions.Navigation2D (Direction2D (..), screenGo, withNavigation2DConfig)
import XMonad.Config.Gnome         (gnomeConfig)
import XMonad.Hooks.DynamicLog     (PP (..), dynamicLogWithPP, shorten, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops   (ewmh, ewmhDesktopsEventHook, ewmhDesktopsStartup, fullscreenEventHook)
import XMonad.Hooks.ManageDocks    (ToggleStruts (..), avoidStruts, manageDocks)
import XMonad.Hooks.UrgencyHook    (NoUrgencyHook (..), withUrgencyHook)
import XMonad.Layout.ComboP        (SwapWindow (..))
import XMonad.Layout.Decoration    (shrinkText)
import XMonad.Layout.Maximize      (maximize, maximizeRestore)
import XMonad.Layout.Named         (nameTail, named)
import XMonad.Layout.NoBorders     (smartBorders)
import XMonad.Layout.Tabbed        (tabbed)
import XMonad.Util.Run             (spawnPipe)
import XMonad.Util.Themes          (smallClean, theme)

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

myModMask = mod4Mask
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor = "#000088"
myFocusedBorderColor = "#00dddd"

------------------------------------------------------------------------
-- Key bindings
--
myKeys conf @ XConfig {XMonad.modMask = modMask} =
  M.fromList $ [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
               , ((modMask, xK_F1), spawn "gmrun")
               , ((modMask, xK_F2), spawn "google-chrome")
               , ((modMask, xK_F3), spawn "~/local/bin/ec")
               , ((modMask, xK_F12), spawn "gnome-screensaver-command --lock")
               , ((modMask .|. shiftMask, xK_F12), spawn "systemctl suspend")
               , ((modMask .|. shiftMask, xK_c), kill)
               , ((modMask, xK_space), sendMessage NextLayout)
               , ((modMask, xK_r), refresh)
               , ((modMask, xK_j), windows W.focusDown)
               , ((modMask, xK_k), windows W.focusUp)
               , ((modMask, xK_m), withFocused $ sendMessage . maximizeRestore)
               , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
               , ((modMask, xK_o), sendMessage SwapWindow)
               , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
               , ((modMask .|. shiftMask,  xK_k), windows W.swapUp)
               , ((modMask .|. shiftMask, xK_h), sendMessage Shrink)
               , ((modMask .|. shiftMask, xK_l), sendMessage Expand)
               , ((modMask, xK_t), withFocused $ windows . W.sink)
               , ((modMask, xK_comma ), sendMessage (IncMasterN 1))
               , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
               , ((modMask, xK_b), sendMessage ToggleStruts)
               , ((0, 0x1008FF11), spawn "amixer set Master 2-")
               , ((0, 0x1008FF13), spawn "amixer set Master 2+")
               , ((0, 0x1008FF12), spawn "amixer set Master toggle")
               , ((modMask .|. shiftMask,  xK_q), io exitSuccess)
               , ((modMask .|. shiftMask,  xK_r), broadcastMessage ReleaseResources >> restart "xmonad" True)
               , ((modMask .|. controlMask, xK_k), screenGo R False)
               , ((modMask .|. controlMask, xK_j), screenGo L False)
               , ((modMask .|. shiftMask, xK_o), shiftNextScreen)
               , ((modMask .|. shiftMask, xK_s), swapNextScreen)
               ]
               ++

               --
               -- mod-[1..6], Switch to workspace N
               -- mod-shift-[1..6], Move client to workspace N
               --
               [((m .|. modMask, k), windows $ f i)
                   | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
                   , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
               ++

               --
               -- mod-{n,p}, Switch to {prev,next} workspace
               -- mod-shift-{n,p}, Move client and shift to {prev,next} workspace
               -- Requires Xmonad.Actions.CycleWS
               --
               [ ((modMask, xK_p), moveTo Prev HiddenWS)
               , ((modMask, xK_n), moveTo Next HiddenWS)
               , ((modMask .|. shiftMask, xK_p), shiftToPrev >> prevWS)
               , ((modMask .|. shiftMask, xK_n), shiftToNext >> nextWS)
               ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings XConfig {XMonad.modMask = modMask} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows W.swapMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
--

myLayout = nameTail $ maximize $ smartBorders $
                named "T" tiled
                ||| named "M" (tabbed shrinkText (theme smallClean))
                ||| named "TH" tiledHorizontal
 where
  -- default tiling algorithm partitions the screen into two panes
  tiled = Tall nmaster delta ratio
  tiledHorizontal = Mirror tiled
  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 1/2
  -- Percent of screen to increment by when resizing panes
  delta   = 3/100

myManageHook = composeAll [ className =? "gmrun" --> doFloat
                          , appName =? "desktop_window" --> doIgnore
                          , className =? "gimp" --> doFloat
                          ]

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn h
                              , ppTitle = xmobarColor "green" "" . shorten 200
                              }

------------------------------------------------------------------------

startup :: X ()
startup = spawn "unity-settings-daemon"

main :: IO ()
main = do
  executeInHome ".xmonad/start.sh"
  workspaceBarPipe <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad . ewmh . withNavigation2DConfig def $ withUrgencyHook NoUrgencyHook (config workspaceBarPipe)
 where
  config wp = gnomeConfig { terminal = "urxvt"
                          , focusFollowsMouse = False
                          , borderWidth = 2
                          , modMask = myModMask
                          , workspaces = myWorkspaces
                          , normalBorderColor = myNormalBorderColor
                          , focusedBorderColor = myFocusedBorderColor
                          , keys = myKeys
                          , mouseBindings = myMouseBindings
                          , manageHook = manageHook gnomeConfig <+> myManageHook <+> manageDocks
                          , logHook = logHook gnomeConfig <+> myLogHook wp
                          , handleEventHook = fullscreenEventHook <+> ewmhDesktopsEventHook <+> handleEventHook gnomeConfig
                          , layoutHook = avoidStruts myLayout
                          , startupHook = startup <+> ewmhDesktopsStartup
                          , clickJustFocuses = False
                          }

  executeInHome file = do
    home <- getHomeDirectory
    doesFileExist (home </> file) >>= \e -> when e . void $ spawn (home </> file)
