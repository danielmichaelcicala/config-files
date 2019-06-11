import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Util.Run
import XMonad.Util.EZConfig

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

import System.IO
import System.Exit

import Graphics.X11.ExtraTypes.XF86
import Data.Monoid
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- border width of focused window
myBorderWidth = 3
-- border color of focused window
myBorderFocusColor = "#0033ff"
-- rebind mod to windows key
myModMask = mod4Mask
-- terminal
myTerminal = "gnome-terminal"
-- workspace names
myWorkspaces = ["1:admin", "2:write", "3:teach", "4:web", "5:media", "6:term", "7", "8", "9" ]
-- location of xmobar config
myXmobarrc = "~/.xmobarrc"

-- =====================
--  custom key bindings
-- =====================

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Lock the screen using command specified by myScreensaver.
  -- , ((modMask .|. controlMask, xK_l),
  --   spawn myScreensaver)

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  -- , ((modMask .|. shiftMask, xK_p),
  --    spawn mySelectScreenshot)

  -- Take a full screenshot using the command specified by myScreenshot.
  -- , ((modMask .|. controlMask .|. shiftMask, xK_p),
  --    spawn myScreenshot)

  -- Mute volume.
  , ((0, xF86XK_AudioMute),
    spawn "amixer -q -D pulse sset Master toggle")

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
    spawn "amixer sset Speaker 5%-")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "amixer sset Speaker 5%+")

  -- Mute volume.
  --, ((modMask .|. controlMask, xK_m),
  --   spawn "amixer -q set Master toggle")

  -- Decrease volume.
  -- , ((modMask .|. controlMask, xK_j),
  --   spawn "amixer -q set Master 5%-")

  -- Increase volume.
  -- , ((modMask .|. controlMask, xK_k),
  --  spawn "amixer -q set Master 5%+")

  -- mute button
  -- , ((modMask .|. controlMask, xK_m), spawn "pulse-volume.sh toggle")

  -- volumeup button
  -- , ((modMask .|. controlMask, xK_k), spawn "pulse-volume.sh increase")

 -- volumedown button
  -- , ((modMask.|. controlMask, xK_j), spawn "pulse-volume.sh decrease")
  
  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown)

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp)

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
   , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]

-- ===============
-- Mouse bindings
-- ===============

-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))
  ]

-- ====================
--  Colors and borders
-- ====================

myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab
-- when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "red"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "red"

myLogHook = do
  dynamicLogWithPP xmobarPP

-- layout
myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

-- ===================
--  THE MAIN FUNCTION
-- ===================
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- key binding to toggle gap for the bar
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- command to launch the bar
myBar = "xmobar"
-- xmobar config variables. they control appearance
-- of text xmonad sends to xmobar via DynamicLog hook
myTitleColor     = "orange" -- color of window title
myTitleLength    = 20 -- truncate window title to this length
myCurrentWSColor = "#e6744c" -- color of active workspace
myVisibleWSColor = "#c185a7" -- color of inactive workspace
myUrgentWSColor  = "#cc0000" -- color of workspace with 'urgent' window
myCurrentWSLeft  = "[" -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft  = "(" -- wrap inactive workspace with these
myVisibleWSRight = ")"
-- custom PP determins what is written to the bar
myPP = xmobarPP 
  { ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
  , ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- my configuration.
myConfig = defaultConfig
      -- hooks, layouts
      { manageHook = manageDocks <+> manageHook defaultConfig
      , layoutHook = avoidStruts $ myLayout
      , logHook = myLogHook
      -- simple stuff
      , terminal = myTerminal
      , borderWidth = myBorderWidth
      , modMask = myModMask
      , workspaces = myWorkspaces
--      , keys = myKeys
      }
      `additionalKeys`
      [
        -- Mute volume.
--        ((0, 0x1008FF12), spawn "amixer -q sset Master toggle")
        -- XF86XK_AudioLowerVolume
        ((0 , 0x1008ff11), spawn "amixer -q sset Master 5%-")
        -- XF86XK_AudioRaiseVolume
      , ((0 , 0x1008ff13), spawn "amixer -q sset Master 5%+")
      ---- increase brightness
      , ((0, 0x2600001), spawn "xbacklight -inc 5")
      ---- decrease brightness
      , ((mod4Mask .|. shiftMask, xK_Down), spawn "xbacklight -dec 5")
      , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command")
      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0, xK_Print), spawn "scrot")
      ]

