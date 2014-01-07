-- {- xmonad.hs
-- - Author: Tim Hermans
-- - Version: 0.0.9
-- -}

-------------------------------------------------------------------------------
-- Imports --
-- core stuff
import Control.Monad (liftM)
import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import System.IO
import System.Random
import System.Exit
import Graphics.X11.Xlib
-- import IO (Handle, hPutStrLn)

import qualified Data.Map as M
import qualified Data.List as L
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))

-- utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Themes
import XMonad.Util.Loggers
import XMonad.Util.EZConfig

-- prompts
import XMonad.Prompt
import XMonad.Prompt.Shell

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.DwmPromote
import XMonad.Actions.Warp
import XMonad.Actions.GridSelect

-- hooks
import XMonad.ManageHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog hiding (shorten)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

-- layouts
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ShowWName
import XMonad.Layout.Magnifier
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.IndependentScreens

-----------------------------------------------------------------
-- Settings

myTerminal                      = "urxvtc"
myModMask                       = mod4Mask
-- myWorkspaces                    = clickable . map dzenEscape $ nWorkspaces 0 [ "main", "emacs", "web", "im", "mail" ]
 --                                      where nWorkspaces n [] = map show [1 .. n]
   --                                          nWorkspaces n l  = init l ++ map show [length l .. n] ++ [last l]
     --                                        clickable l      = [ "^ca(1,xdotool key super+" ++ show n ++ ")" ++ ws ++ "^ca()" | (i,ws) <- zip [1..] l, let n = i ]
myWorkspaces                    = clickable . (map dzenEscape) $ [ "main", "emacs", "web", "im", "mail" ]
                                        where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" | (i,ws) <- zip [1..] l, let n = i ]

myBorderWidth                   = 2

mySep                           = "|"
myWsSep                         = ""


-- myRootMask                      = substructureRedirectMask .|. substructureNotifyMask
--                                 .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask .|. buttonPressMask

-- myClientMask                    =  structureNotifyMask .|. enterWindowMask .|. propertyChangeMask

-- Icons
myIconDir                       = "/home/thermans/.icons/dzen"
myWorkspaceIcon                 = "corner.xbm"
myWorkspaceUnselIcon            = "square.xbm"

myIcons layout
    | is "Mirror ResizableTall" = Just "layout-mirror-bottom.xbm"
    | is "ResizableTall"        = Just "layout-tall-right.xbm"
    | is "Full"                 = Just "layout-full.xbm"
    | is "ReflectX IM Grid"     = Just "layout-gimp.xbm"
    | otherwise                 = Nothing
  where is                      = (`L.isInfixOf` layout)

-- Colors
myNormalBorderColor             = "#535D6C"
myFocusedBorderColor            = "#333333"

myDzenFGColor                   = "#AAAAAA"
myDzenBGColor                   = "#333333"

myNormalFGColor                 = myDzenFGColor
myNormalBGColor                 = myDzenBGColor

myFocusedFGColor                = "#EE9A00"
myFocusedBGColor                = "#333333"

myTitleFGColor                  = "#90E103"

myUrgentFGColor                 = "#FFFFFF"
myUrgentBGColor                 = "#B33C23"

myVisibleFGColor                = "#FFFFFF"
myVisibleBGColor                = "#333333"

myHiddenFGColor                 = "#0390E1"
myHiddenBGColor                 = ""

myEmptyFGColor                  = "#8B8378"
myEmptyBGColor                  = ""

myDefaultFont                   = "xft:Dejavu Sans:size=9"

-----------------------------------------------------------------
-- Keys
myKeys = \conf -> mkKeymap conf $
                [ ("M-S-<Esc>",    spawn "xkill")
                , ("M-<Space>",    sendMessage NextLayout)
                , ("M-S-<Space>",  setLayout $ XMonad.layoutHook conf)
                , ("M-n",          refresh)
                , ("M-S-c",        kill)
                , ("M-<Tab>",      windows W.focusDown)
                , ("M-j",          windows W.focusDown)
                , ("M-S-<Tab>",    windows W.focusUp)
                , ("M-k",          windows W.focusUp)
                , ("M-m",          windows W.focusMaster)
                , ("M-S-k",        windows W.swapDown)
                , ("M-S-j",        windows W.swapUp)
                , ("M-b",          sendMessage ToggleStruts)
                , ("M-h",          sendMessage Shrink)
                , ("M-l",          sendMessage Expand)
                , ("M-t",          withFocused $ windows . W.sink)
                , ("M-,",          sendMessage (IncMasterN 1))
                , ("M-.",          sendMessage (IncMasterN (-1)))
                , ("M-S-q",        io (exitWith ExitSuccess))
                  -- , ("M-q",          restart "xmonad" True)
                , ("M-q",          spawn "xmonad --recompile; xmonad --restart")
                , ("M-p",          shellPrompt myXPConfig)
                , ("M-S-<Right>",  shiftToNext >> nextWS)
                , ("M-S-<Left>",   shiftToPrev >> prevWS)
                , ("M-<Down>",     nextScreen)
                , ("M-o",          shiftNextScreen >> nextScreen)
                , ("M-<Left>",     prevNonEmptyWS )
                , ("M-C-k",        prevNonEmptyWS )
                , ("M-<Right>",    nextNonEmptyWS )
                , ("M-C-j",        nextNonEmptyWS )
                , ("M-s",          swapNextScreen)
                , ("M-<Up>",       swapNextScreen)
                , ("M-a",          sendMessage MirrorShrink)
                , ("M-y",          sendMessage MirrorExpand)
                , ("M-S-<Return>", dwmpromote)
                , ("M-u",          focusUrgent)
                , ("M-x M-x",      nextScreen)
                , ("M-x w",        sendMessage MagnifyMore)
                , ("M-x e",        sendMessage MagnifyLess)
               -- grid select
                , ("M-g",          goToSelected defaultGSConfig)
               -- spawn
                , ("M-e",         spawn "emacsclient -n -c")
                , ("M-w",         spawn "firefox")
                , ("M-i",         spawn "pidgin")
                , ("M-<Return>",  spawn $ XMonad.terminal conf)
                -- mpd controls
                , ("M-C-t",        spawn "mpc pause")
                , ("M-C-p",        spawn "mpc play")
                , ("M-C-n",        spawn "mpc next")

                , ("M-z",          warpToWindow (1%10) (1%10)) -- Move pointer to currently focused window
                ]
                ++
                [ (m ++ i, windows $ onCurrentScreen f j)
                -- [ (m ++ i, windows $ f j)
                    | (i, j) <- zip (map show [1..9]) (workspaces conf)
                    , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
                ]
    where
      nextNonEmptyWS = moveTo Next (WSIs (liftM (not .) isVisible))
      prevNonEmptyWS = moveTo Prev (WSIs (liftM (not .) isVisible))

isVisible :: X (WindowSpace -> Bool)
isVisible = do
  vs <- gets (map (W.tag . W.workspace) . W.visible . windowset)
  return (\w -> W.tag w `elem` vs)

-----------------------------------------------------------------
-- Mouse
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows W.swapMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    -- cycle focus
    , ((modMask, button4), \_ -> windows W.focusUp)
    , ((modMask, button5), \_ -> windows W.focusDown)
    -- cycle through workspaces
    , ((controlMask .|. modMask, button5), nextNonEmptyWS)
    , ((controlMask .|. modMask, button4), prevNonEmptyWS)
    ]
    where
      nextNonEmptyWS _ = moveTo Next (WSIs (liftM (not .) isVisible))
      prevNonEmptyWS _ = moveTo Prev (WSIs (liftM (not .) isVisible))

-----------------------------------------------------------------
-- Hooks --
myManageHook = composeAll
    [
        className=? "Pidgin"                    --> doShift "im"
      , className=? "Uzbl-core"                 --> doShift "web"
      , className=? "Firefox"                   --> doShift "web"
      , className=? "Emacs"                     --> doShift "emacs"
      , className=? "Synapse"                   --> doIgnore
      , isDialog                                --> placeHook simpleSmart <+> doFloat
    ]
      -- <+> positionStoreManageHook
      <+> manageDocks

myLayoutHook =
  avoidStruts $
  onWorkspace "im" (reflectHoriz . withIM 0.20 (Role "buddy_list") $ Grid) $
  onWorkspace "emacs" tiled $
  onWorkspace "mail" simpleFloat $
  smartBorders (Mirror tiled)
  ||| Grid
  ||| smartBorders Full
  ||| smartBorders tiled
  where
    tiled = ResizableTall nmaster delta ratio []
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = toRational (2/(1+sqrt(5)::Double)) -- golden
    -- Percent of screen to increment by when resizing panes
    delta   = 0.03

------------------------------------------------------------------
-- Prompt --
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {
               font              = "xft:Consolas-12"
             , bgColor           = myDzenBGColor
             , fgColor           = myDzenFGColor
             , fgHLight          = "orange"
             , bgHLight          = "#535D6C"
             , borderColor       = "black"
             , promptBorderWidth = 1
             , position          = Top
             , height            = 24
             , defaultText       = []
             }

-----------------------------------------------------------------
-- Themes --

-- for Tabbed layouts
myTabTheme :: Theme
myTabTheme = defaultTheme {
               activeBorderColor   = myFocusedBorderColor
             , inactiveBorderColor = myNormalBorderColor
             , activeColor         = myFocusedBGColor
             , activeTextColor     = myFocusedFGColor
             , inactiveColor       = myHiddenBGColor
             , inactiveTextColor   = myHiddenFGColor
             , fontName            = myDefaultFont
             , decoHeight          = 20
             , urgentColor         = myUrgentBGColor
             , urgentTextColor     = myUrgentFGColor
             }

-----------------------------------------------------------------
-- Status Bars

topbarCmd = "dzen2 -bg '" ++ myDzenBGColor ++ "' -x 0 -y 0 -fg '" ++ myDzenFGColor ++"' -ta l -sa l -e '' -fn '" ++ myDefaultFont ++ "'"

myLogHook top = do
  ewmhDesktopsLogHook
  dynamicLogXinerama
  updatePointer (Relative (1/20) (1/20))
 -- fadeInactiveLogHook fadeAmount
 --   where fadeAmount = 0.93
  dynamicLogWithPP $ defaultPP {
                         -- ppLayout          = dzenColor myNormalFGColor myNormalBGColor . pad . loadIcons
                         ppLayout          = dzenColor myNormalFGColor myNormalBGColor . pad
                       -- , ppCurrent         = dzenColor myFocusedFGColor myFocusedBGColor .pad . squareIcon
                       , ppCurrent         = dzenColor myFocusedFGColor myFocusedBGColor . pad
                       , ppVisible         = dzenColor myVisibleFGColor myVisibleBGColor
                       , ppHidden          = dzenColor myHiddenFGColor myHiddenBGColor . pad
                       -- , ppHidden          = dzenColor myHiddenFGColor myHiddenBGColor . pad . cornerIcon
                       , ppHiddenNoWindows = dzenColor myEmptyFGColor myEmptyBGColor . pad
                       , ppTitle           = dzenColor myTitleFGColor "" . wrap "[" "]" . pad
                       , ppSep             = mySep
                       , ppWsSep           = myWsSep
                       , ppUrgent          = dzenColor myUrgentFGColor myUrgentBGColor
                       , ppOutput          = hPutStrLn top
                       }
      -- where loadIcons s = fromMaybe s $ myIcons s >>= \icon -> return $ "^i(" ++ myIconDir ++ "/" ++ icon ++ ")"
      --       cornerIcon  = (++) $ "^i(" ++ myIconDir ++ "/" ++ myWorkspaceIcon ++ ")"
      --       squareIcon  = (++) $ "^i(" ++ myIconDir ++ "/" ++ myWorkspaceUnselIcon ++ ")"
      --       shorten :: Int -> String -> String
      --       shorten n xs | length xs < n = xs
      --                    | otherwise     = (take (n - length end) xs) ++ end
      --                    where
      --                      end = "…"

myStartupHook = ewmhDesktopsStartup

myEventHook = ewmhDesktopsEventHook

-- Focus follows mouse
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-----------------------------------------------------------------
-- Main
main = do
  bottom <- spawnPipe "xmobar"
  top   <- spawnPipe topbarCmd
  xmonad . withUrgencyHookC NoUrgencyHook (urgencyConfig { suppressWhen = Focused }) . ewmh $ XConfig {
                    -- basics
                    terminal           = myTerminal,
                     workspaces         = withScreens 2 myWorkspaces,
                    {-workspaces         =  myWorkspaces,-}
                    modMask            = myModMask,
                    borderWidth        = myBorderWidth,
                    normalBorderColor  = myNormalBorderColor,
                    focusedBorderColor = myFocusedBorderColor,
                    focusFollowsMouse  = True,
                    clickJustFocuses   = True,
                    --rootMask           = myRootMask,
                    -- clientMask         = myClientMask,
                    -- key bindings
                    keys               = myKeys,
                    mouseBindings      = myMouseBindings,
                    -- hooks, layouts
                    logHook            = myLogHook top,
                    layoutHook         = showWName myLayoutHook,
                    manageHook         = myManageHook,
                    handleEventHook    = myEventHook,
                    startupHook        = myStartupHook
                }
