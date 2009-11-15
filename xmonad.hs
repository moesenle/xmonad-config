--
-- xmonad config file.
--
 
-- XMonad Core
import XMonad
import System.Exit
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
 
-- GHC hierarchical libraries
import XMonad.Operations
import XMonad.Config
import XMonad.Util.Run
import System.IO
import Data.Ratio ((%))
 
--Contribs
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders
 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
 
-- import XMonad.Layout
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Tabbed

import XMonad.Util.Loggers
import XMonad.Util.Timer
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Themes
 
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 2
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask	= mod4Mask
altMask 	= mod1Mask 
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000088"
myFocusedBorderColor = "#dddddd"
 
------------------------------------------------------------------------
-- Key bindings
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask,                xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch gmrun
    , ((modMask,		xK_F1	), spawn "gmrun")
 
    -- launch firefox
    , ((modMask,	        xK_F2	), spawn "firefox -new-window")

    -- launch emacs
    , ((modMask,	        xK_F3	), spawn "emacsclient.emacs-snapshot -c -a emacs-snapshot")

    -- lock screen
    , ((modMask,	        xK_F12	), spawn "gnome-screensaver-command --lock")      
 
    -- close focused window
    , ((modMask .|. shiftMask,  xK_c    ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,                xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    -- , ((modMask .|. shiftMask,  xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,                xK_r     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,                xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,                xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,                xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modMask,             xK_m     ), withFocused $ sendMessage . maximizeRestore )
 
    -- Swap the focused window and the master window
    , ((modMask .|. shiftMask,  xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask,  xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask,  xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask,                xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,                xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,                xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask,                xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask,                xK_period), sendMessage (IncMasterN (-1)))
 
    -- toggle the status bar gap
    , ((modMask,                xK_b     ), sendMessage ToggleStruts)
 
    -- Quit xmonad (Default)
    , ((modMask .|. shiftMask,  xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modMask .|. shiftMask,  xK_r     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]
    ++
 
    --
    -- mod-[1..6], Switch to workspace N
    -- mod-shift-[1..6], Move client to workspace N
    --
 
    [((m .|. modMask, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{n,p}, Switch to {prev,next} workspace
    -- mod-shift-{n,p}, Move client and shift to {prev,next} workspace
    
    -- Requires Xmonad.Actions.CycleWS
    --
    [	((modMask		, xK_p	), do t <- findWorkspace getSortByIndex Prev AnyWS 2
                                              windows . W.view $ t)
      ,	((modMask		, xK_n	), do t <- findWorkspace getSortByIndex Next AnyWS 2
                                              windows . W.view $ t)
      ,	((modMask .|. shiftMask	, xK_p	), do t <- findWorkspace getSortByIndex Prev AnyWS 2
                                              windows . W.shift $ t)
    ,	((modMask .|. shiftMask	, xK_n	), do t <- findWorkspace getSortByIndex Next AnyWS 2
                                              windows . W.shift $ t)
    ]
    ++

    --
    -- mod-{w,e}, Switch to physical/Xinerama screens 1, 2
    -- mod-shift-{w,e}, Move client to screen 1, 2
    --
    -- [((m .|. modMask .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_j, xK_k] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    [((modMask .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . W.view))
        | (key, sc) <- zip [xK_j, xK_k] [0..]]
    ++
    [ ((modMask, xK_o), shiftNextScreen) ]
 
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
-- 
 
genericLayout =	maximize $
                tiled 
	        ||| Mirror tiled 
                ||| tabbed shrinkText (theme smallClean)
	        ||| simpleFloat
                
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
  
myLayout = onWorkspace "0_1" simpleFloat $ genericLayout
 
------------------------------------------------------------------------
-- Window rules:
--
myManageHook = composeAll
    [ className =? "gmrun"		--> doFloat,
      resource =? "desktop_window"	--> doIgnore,
      className =? "Skype"              --> doFloat,
      className =? "Kopete"             --> doFloat,
      className =? "Kicker"             --> doFloat,
      isKDETrayWindow                   --> doIgnore
    ]
 
 
-- Whether focus follows the mouse pointer.
--
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
 
-- Dzen stuff
myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ xmobarPP
                       {
                         ppOutput = hPutStrLn h,
                         ppTitle = xmobarColor "green" "" . shorten 120,
                         ppCurrent = xmobarColor "blue" "#ffffff" . separateScreens,
                         ppVisible = xmobarColor "blue" "#afafaf" . separateScreens,
                         ppHidden = separateScreens,
                         ppHiddenNoWindows = xmobarColor "#909090" "" . separateScreens,
                         ppUrgent = xmobarColor "red" "" . separateScreens,
                         ppSort = mkWsSort compareIndependentScreens
                       }
  where
    separateScreens = screenSeparatorWithIndex 1 "1" $ xmobarColor "gray" "" "| "
    compareIndependentScreens = do
      return $ \a b -> f (unmarshall a) (unmarshall b)
    
    f (s1, _) (s2, _) | s1 < s2 = LT
    f (s1, _) (s2, _) | s2 < s1 = GT
    f (_, d1) (_, d2) = compare d1 d2

    screenSeparatorWithIndex :: ScreenId -> String -> String -> PhysicalWorkspace -> String
    screenSeparatorWithIndex si ws sep pw = if ws_si == si && vi_ws == ws then sep ++ vi_ws else vi_ws
      where (ws_si, vi_ws) = unmarshall pw
 
------------------------------------------------------------------------
 
main :: IO ()
main = do
  workspaceBarPipe <- spawnPipe "/usr/wiss/moesenle/local/private/lenny-amd64/bin/xmobar /usr/wiss/moesenle/.xmonad/xmobarrc"
                           
  --  conkyBarPipe <- spawnPipe myConkyBar
  xmonad $ withUrgencyHook NoUrgencyHook defaultConfig {
       -- simple stuff
       terminal           = myTerminal,
       focusFollowsMouse  = myFocusFollowsMouse,
       borderWidth        = myBorderWidth,
       modMask            = myModMask,
       numlockMask        = myNumlockMask,
       workspaces         = withScreens 2 myWorkspaces,
       normalBorderColor  = myNormalBorderColor,
       focusedBorderColor = myFocusedBorderColor,
 
       -- key bindings
       keys               = myKeys,
       mouseBindings      = myMouseBindings,
 
       -- hooks, layouts
       manageHook         = myManageHook <+> manageDocks <+> manageHook defaultConfig,
       logHook	          = myLogHook workspaceBarPipe,
       handleEventHook    = ewmhDesktopsEventHook,
 
       -- For use with no panels or just dzen2
       layoutHook         = avoidStruts $ myLayout
    }
