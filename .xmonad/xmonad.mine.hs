-- Options
 {-# LANGUAGE
     DeriveDataTypeable,
     FlexibleContexts,
     FlexibleInstances,
     MultiParamTypeClasses,
     NoMonomorphismRestriction,
     PatternGuards,
     ScopedTypeVariables,
     TypeSynonymInstances,
     UndecidableInstances
     #-}

{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}
 
-- {- xmonad.hs
--  - Author: Tim Hermans
--  - Version: 0.0.9
-- -}

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+- &IMPORTS +--

-- core stuff
import Control.Monad (liftM)
import XMonad
import qualified XMonad.StackSet as W
import System.IO
import System.Exit

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))

-- utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Loggers
import XMonad.Util.EZConfig
import Graphics.X11.Xinerama
import Control.Exception as E
import Control.Applicative
import XMonad.Util.Scratchpad
-- import XMonad.Util.NamedScratchpad

-- prompts
import XMonad.Prompt
import XMonad.Prompt.Shell

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.DwmPromote
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo
import XMonad.Actions.GridSelect

-- hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog hiding (shorten)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

-- layouts
import XMonad.Layout
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Magnifier

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+- &MAIN +--
main :: IO ()
main = do
  r <- getScreenRes ":0" 0  --display ":0", screen 0
  bottom <- dzenSpawnPipe $ dzenBotRightFlags r
  topLeft   <- dzenSpawnPipe $ dzenTopLeftFlags r
  topRight   <- dzenSpawnPipe $ dzenTopRightFlags r
  xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] } . ewmh $ XConfig {
                    -- basics
                    terminal           = myTerminal,
                    workspaces         = myWorkspaces,
                    modMask            = myModMask,
                    borderWidth        = myBorderWidth,
                    normalBorderColor  = myNormalBorderColor,
                    focusedBorderColor = myFocusedBorderColor,
                    focusFollowsMouse  = True,
                    clickJustFocuses   = True,
                    -- key bindings
                    keys               = myKeys,
                    mouseBindings      = myMouseBindings,
                    -- hooks, layouts
                    logHook            = myTopLeftLogHook topLeft   <+>
                                         myTopRightLogHook topRight <+>
                                         myBotRightLogHook bottom, 
                    layoutHook         = myLayoutHook,
                    manageHook         = manageDocks <+> myManageHook,
                    handleEventHook    = myEventHook,
                    startupHook        = myStartupHook
                }

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ &SETTINGS +--
myTerminal = "urxvtc"

myModMask  = mod4Mask

myWorkspaces =
  [
    "main"
  , "emacs"
  , "web"
  , "im"
  , "music"
  ]

myBorderWidth                   = 1

mySep                           = "|"

myWsSep                         = ""

-- Icons
myIconDir                       = "/home/thermans/.icons"
myWorkspaceIcon                 = "corner.xbm"
myWorkspaceUnselIcon            = "square.xbm"

myIcons layout
    | is "Mirror ResizableTall" = Just "layout-mtall.xbm"
    | is "ResizableTall"        = Just "layout-tall.xbm"
    | is "Full"                 = Just "layout-full.xbm"
    | is "ReflectX IM Grid"     = Just "layout-im.xbm"
    | is "Grid"                 = Just "layout-grid.xbm"
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

myDefaultFont                   = "xft:DejaVu Sans:size=9"

-- Colors, fonts and paths
-- dzenFont       = "-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
dzenFont       = "xft:DejaVu Sans:size=9"
colorBlack     = "#020202" --Background
colorBlackAlt  = "#1c1c1c" --Black Xdefaults
colorGray      = "#444444" --Gray
colorGrayAlt   = "#101010" --Gray dark
colorGrayAlt2  = "#404040"
colorGrayAlt3  = "#252525"
colorWhite     = "#a9a6af" --Foreground
colorWhiteAlt  = "#9d9d9d" --White dark
colorWhiteAlt2 = "#b5b3b3"
colorWhiteAlt3 = "#707070"
colorMagenta   = "#8e82a2"
colorBlue      = "#44aacc"
colorBlueAlt   = "#3955c4"
colorRed       = "#f7a16e"
colorRedAlt    = "#e0105f"
colorGreen     = "#66ff66"
colorGreenAlt  = "#558965"
boxLeftIcon    = "/home/thermans/.icons/xbm_icons/subtle/boxleft.xbm"  --left icon of dzen boxes
boxLeftIcon2   = "/home/thermans/.icons/xbm_icons/subtle/boxleft2.xbm" --left icon2 of dzen boxes
boxRightIcon   = "/home/thermans/.icons/xbm_icons/subtle/boxright.xbm" --right icon of dzen boxes

xDefRes        = 1920   -- X resolution
yDefRes        = 1000   -- Y resolution
panelHeight    = 16     --height of top and bottom panels
boxHeight      = 14     --height of dzen logger box
topPanelSepPos = 1500   --left-right alignment pos of top panel
botPanelSepPos = 0      --left-right alignment pos of bottom panel

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+- &KEYS +--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
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
                -- , ("M-t",          withFocused $ windows . W.sink)
                , ("M-,",          sendMessage (IncMasterN 1))
                , ("M-.",          sendMessage (IncMasterN (-1)))
                , ("M-S-q",        io (exitWith ExitSuccess))
                , ("M-q",          spawn "xmonad --recompile; xmonad --restart")
                , ("M-p",          shellPrompt myXPConfig)
                , ("M-S-<Right>",  shiftToNext >> nextWS)
                , ("M-S-<Left>",   shiftToPrev >> prevWS)
                , ("M-<Left>",     prevNonEmptyWS )
                , ("M-C-k",        prevNonEmptyWS )
                , ("M-<Right>",    nextNonEmptyWS )
                , ("M-C-j",        nextNonEmptyWS )
                , ("M-S-<Return>", dwmpromote)
                , ("M-u",          focusUrgent)
               -- grid select
                , ("M-g",          goToSelected defaultGSConfig)
               -- spawn
                , ("M-e",         spawn "emacsclient -n -c")
                , ("M-w",         runOrRaise "firefox"           (className =? "Firefox"))
                , ("M-i",         runOrRaise "pidgin"            (className =? "Pidgin"))
                , ("M-<Return>",  spawn $ XMonad.terminal conf)
                , ("M-t",         scratchPad)
                -- mpd controls
                , ("M-C-t",        spawn "mpc pause")
                , ("M-C-p",        spawn "mpc play")
                , ("M-C-n",        spawn "mpc next")
                ]
                ++
                [ (m ++ i, windows $ f j)
                -- [ (m ++ i, windows $ f j)
                    | (i, j) <- zip (map show [1..9]) (workspaces conf)
                    , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
                ]
    where
      scratchPad = scratchpadSpawnActionCustom "/usr/bin/urxvtc -name scratchpad"
      nextNonEmptyWS = moveTo Next (WSIs (liftM (not .) isVisible))
      prevNonEmptyWS = moveTo Prev (WSIs (liftM (not .) isVisible))

isVisible :: X (WindowSpace -> Bool)
isVisible = do
  vs <- gets (map (W.tag . W.workspace) . W.visible . windowset)
  return (\w -> W.tag w `elem` vs)

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ &MOUSE +--
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
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

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ &HOOKS +--
myManageHook :: ManageHook
myManageHook =
  manageDocks <+>
  (scratchpadManageHook $ W.RationalRect 0.3 0 0.4 0.3) <+>
  manageWindows

manageWindows :: ManageHook
manageWindows = composeAll
    [
        className=? "Pidgin"                    --> doShift "im"
      , classNotRole ("Pidgin", "buddy_list")   --> doFloat
      , className=? "Uzbl-core"                 --> doShift "web"
      , className=? "luakit"                    --> doShift "web"
      , className=? "Dwb"                       --> doShift "web"
      , className=? "Firefox"                   --> doShift "web"
      , className=? "Emacs"                     --> doShift "emacs"
      , className=? "Clementine"                --> doShift "music"
      , className=? "Synapse"                   --> doIgnore
      -- , isDialog                                --> placeHook simpleSmart <+> doFloat
      , className=? "Pavucontrol"               --> doCenterFloat
      , className=? "Gparted"                   --> doCenterFloat
      , isDialog                                --> doCenterFloat
    ]
  where
        classNotRole :: (String, String) -> Query Bool
        classNotRole (c,r) = className =? c <&&> role /=? r

        role = stringProperty "WM_WINDOW_ROLE"

-- urgencyHook
-- myUrgencyHook :: LayoutClass l Window => XConfig l -> XConfig l
-- -- myUrgencyHook :: XConfig l0
-- myUrgencyHook = withUrgencyHook dzenUrgencyHook
-- 	{ duration = 200000
-- 	, args =
-- 		[ "-x", "0"
-- 		, "-y", "0"
-- 		, "-h", show panelHeight
-- 		, "-w", show topPanelSepPos
-- 		, "-fn", dzenFont
-- 		, "-bg", colorBlack
-- 		, "-fg", colorGreen
-- 		]
-- 	}
 
-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+-- &LAYOUT +--
myLayoutHook =
  avoidStruts $
  onWorkspace "im" (reflectHoriz . withIM 0.10 (Role "buddy_list") $ Grid) $
  onWorkspace "emacs" tiled $
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

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+- &PROMPTS +--
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

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+-- &THEMES +--
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
defaultBoxPP :: BoxPP
defaultBoxPP = BoxPP
	{ bgColorBPP   = myNormalBGColor
	, fgColorBPP   = myNormalFGColor
  , boxColorBPP  = myNormalBGColor
	, leftIconBPP  = boxLeftIcon
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}

gray2BoxPP :: BoxPP
gray2BoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorGray
	, boxColorBPP  = colorGrayAlt
	, leftIconBPP  = boxLeftIcon2
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}
 
blueBoxPP :: BoxPP
blueBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlue
	, boxColorBPP  = colorGrayAlt
	, leftIconBPP  = boxLeftIcon
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}
 
blue2BoxPP :: BoxPP
blue2BoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlue
	, boxColorBPP  = colorGrayAlt
	, leftIconBPP  = boxLeftIcon2
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}
 
whiteBoxPP :: BoxPP
whiteBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorWhiteAlt
	, boxColorBPP  = colorGrayAlt
	, leftIconBPP  = boxLeftIcon
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}
 
blackBoxPP :: BoxPP
blackBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlack
	, boxColorBPP  = colorGrayAlt
	, leftIconBPP  = boxLeftIcon
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}
 
white2BBoxPP :: BoxPP
white2BBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlack
	, boxColorBPP  = colorWhiteAlt
	, leftIconBPP  = boxLeftIcon2
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}
 
blue2BBoxPP :: BoxPP --current workspace
blue2BBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlack
	, boxColorBPP  = colorBlue
	, leftIconBPP  = boxLeftIcon2
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}
 
green2BBoxPP :: BoxPP --urgent workspace
green2BBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlack
	, boxColorBPP  = colorGreen
	, leftIconBPP  = boxLeftIcon2
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+- &STATUSBARS +--
dzenTopLeftFlags :: Res -> DF
dzenTopLeftFlags _ = DF
	{ xPosDF       = 0
	, yPosDF       = 0
	, widthDF      = topPanelSepPos
	, heightDF     = panelHeight
	, alignmentDF = "l"
        , fgColorDF    = myDzenFGColor
	, bgColorDF    = myDzenBGColor
	, fontDF       = myDefaultFont
	, eventDF      = "onstart=lower"
	, extrasDF     = "-p"
	}

-- Top left bar logHook
myTopLeftLogHook :: Handle -> X ()
myTopLeftLogHook top = dynamicLogWithPP defaultPP
	{
                ppLayout          = dzenColor myNormalFGColor myNormalBGColor . pad . loadIcons
              , ppCurrent         = dzenColor myFocusedFGColor myFocusedBGColor . pad . squareIcon
              , ppVisible         = dzenColor myVisibleFGColor myVisibleBGColor . pad . dzenClickWorkspace
              , ppHidden          = dzenColor myHiddenFGColor myHiddenBGColor . pad . cornerIcon . dzenClickWorkspace
              , ppHiddenNoWindows = dzenColor myEmptyFGColor myEmptyBGColor . pad . dzenClickWorkspace
              , ppTitle           = dzenColor myTitleFGColor "" . wrap "[" "]" . pad
              , ppSep             = mySep
              , ppWsSep           = myWsSep
              , ppUrgent          = dzenColor myUrgentFGColor myUrgentBGColor . dzenClickWorkspace
              , ppOutput          = hPutStrLn top
              }
  where loadIcons s = fromMaybe s $ myIcons s >>= \icon -> return $ "^i(" ++ myIconDir ++ "/" ++ icon ++ ")" 
        cornerIcon = (++) $ "^i(" ++ myIconDir ++ "/" ++ myWorkspaceIcon ++ ")"
        squareIcon = (++) $ "^i(" ++ myIconDir ++ "/" ++ myWorkspaceUnselIcon ++ ")"
        dzenClickWorkspace ws = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "w;" ++ xdo index ++ ")" ++ ws ++ "^ca()^ca()" where
			wsIdxToString Nothing = "1"
			wsIdxToString (Just n) = show $ mod (n+1) $ length myWorkspaces
			index = wsIdxToString (L.elemIndex ws myWorkspaces)
			xdo key = "/usr/bin/xdotool key super+" ++ key

dzenTopRightFlags :: Res -> DF
dzenTopRightFlags r = DF
	{ xPosDF       = topPanelSepPos
	, yPosDF       = 0
	, widthDF      = (xRes r) - topPanelSepPos
	, heightDF     = panelHeight
	, alignmentDF = "r"
	, fgColorDF    = myNormalFGColor
	, bgColorDF    = myNormalBGColor
	, fontDF       = dzenFont
	, eventDF      = "onstart=lower"
	, extrasDF     = "-p"
	}

-- Top right bar logHook
myTopRightLogHook :: Handle -> X ()
myTopRightLogHook h = dynamicLogWithPP defaultPP
	{ ppOutput  = hPutStrLn h
	, ppOrder   = \(_:_:_:x) -> x
	, ppSep     = " "
	, ppExtras  = [ myDateL ]
	}

-- Dzen bottom right bar flags
dzenBotRightFlags :: Res -> DF
dzenBotRightFlags r = DF
	{ xPosDF       = 0
	, yPosDF       = yRes r - panelHeight
	-- , yPosDF       = yDefRes - panelHeight
        , widthDF      = 1872
	--, widthDF      = xDefRes
	, heightDF     = panelHeight
	, alignmentDF = "r"
	, fgColorDF    = myDzenFGColor
	, bgColorDF    = myDzenBGColor
	, fontDF       = myDefaultFont
	, eventDF      = "onstart=lower"
	, extrasDF     = "-p"
	}

-- Bottom right bar logHook
myBotRightLogHook :: Handle -> X ()
myBotRightLogHook h = dynamicLogWithPP defaultPP
	{ ppOutput = hPutStrLn h
	, ppOrder  = \(_:_:_:x) -> x
	, ppSep    = " "
	, ppExtras = [ myMemL ]
	}
 

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+- &LOGGERS +--
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
                       , ppCurrent         = dzenColor myFocusedFGColor myFocusedBGColor .pad
                       , ppVisible         = dzenColor myVisibleFGColor myVisibleBGColor
                       -- , ppHidden          = dzenColor myHiddenFGColor myHiddenBGColor . pad . cornerIcon
                       , ppHidden          = dzenColor myHiddenFGColor myHiddenBGColor . pad
                       , ppHiddenNoWindows = dzenColor myEmptyFGColor myEmptyBGColor . pad
                       , ppTitle           = dzenColor myTitleFGColor "" . wrap "[" "]" . pad
                       , ppSep             = mySep
                       , ppWsSep           = myWsSep
                       , ppUrgent          = dzenColor myUrgentFGColor myUrgentBGColor
                       , ppOutput          = hPutStrLn top
                       } where
                               dzenClickWorkspace ws = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "w;" ++ xdo index ++ ")" ++ ws ++ "^ca()^ca()" where
			           wsIdxToString Nothing = "1"
			           wsIdxToString (Just n) = show $ mod (n+1) $ length myWorkspaces
			           index = wsIdxToString (L.elemIndex ws myWorkspaces)
			           xdo key = "/usr/bin/xdotool key super+" ++ key
 

      -- where loadIcons s = fromMaybe s $ myIcons s >>= \icon -> return $ "^i(" ++ myIconDir ++ "/" ++ icon ++ ")"
      --       cornerIcon = (++) $ "^i(" ++ myIconDir ++ "/" ++ myWorkspaceIcon ++ ")"
      --       squareIcon = (++) $ "^i(" ++ myIconDir ++ "/" ++ myWorkspaceUnselIcon ++ ")"

myStartupHook = ewmhDesktopsStartup

myEventHook = ewmhDesktopsEventHook

-- Concat two Loggers
(++!) :: Logger -> Logger -> Logger
l1 ++! l2 = (liftA2 . liftA2) (++) l1 l2
 
-- Label
labelL :: String -> Logger
labelL = return . return
 
-- Init version for Logger
initL :: Logger -> Logger
initL = (fmap . fmap) initNotNull
 
-- Concat a list of loggers
concatL :: [Logger] -> Logger
concatL [] = return $ return ""
concatL (x:xs) = x ++! concatL xs

-- Concat a list of loggers with spaces between them
concatWithSpaceL :: [Logger] -> Logger
concatWithSpaceL [] = return $ return ""
concatWithSpaceL (x:xs) = x ++! (labelL " ") ++! concatWithSpaceL xs

initNotNull :: String -> String
initNotNull [] = "0\n"
initNotNull xs = init xs
 
tailNotNull :: [String] -> [String]
tailNotNull [] = ["0\n"]
tailNotNull xs = tail xs

-- Convert the content of a file into a Logger
fileToLogger :: (String -> String) -> String -> FilePath -> Logger
fileToLogger f e p = do
	let readWithE f1 e1 p1 = E.catch (do
		contents <- readFile p1
		return $ f1 (initNotNull contents) ) ((\_ -> return e1) :: E.SomeException -> IO String)
	str <- liftIO $ readWithE f e p
	return $ return str

myDateL =
	(dzenBoxStyleL defaultBoxPP   $ date $ "%A %b %d, %Y %l:%M %p")


myMemL =
	(dzenBoxStyleL defaultBoxPP $ labelL "Memory ") ++!
	(dzenBoxStyleL blueBoxPP  $ memUsage [percMemUsage])
	-- (dzenBoxStyleL blueBoxPP  $ memUsage [percMemUsage, totMBMemUsage])

-- Memory usage
memUsage :: [(String -> String)] -> Logger
memUsage xs = initL $ concatWithSpaceL $ map funct xs where
	funct x = fileToLogger x "N/A" "/proc/meminfo"
 
_memUsed x = (_memValues x !! 0) - ((_memValues x !! 2) + (_memValues x !! 3) + (_memValues x !! 1))
_memPerc x = div (_memUsed x * 100) (_memValues x !! 0)
_memValues x = map (getValues x) $ take 4 [0..] where
	getValues x n = read (words (lines x !! n) !! 1)::Int
 
freeBMemUsage x = (show $ _memValues x !! 1) ++ "B"
freeMBMemUsage x = (show $ div (_memValues x !! 1) 1024) ++ "MB"
totBMemUsage = (++"B") . show . _memUsed
totMBMemUsage = (++"MB") . show . (`div` 1024) . _memUsed
percMemUsage = (++"%") . show . _memPerc
 

-- -+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+- &DZENUTILS +--
data DF = DF
	{ xPosDF       :: Int
	, yPosDF       :: Int
	, widthDF      :: Int
	, heightDF     :: Int
	, alignmentDF :: String
	, fgColorDF    :: String
	, bgColorDF    :: String
	, fontDF       :: String
	, eventDF      :: String
	, extrasDF     :: String
	}

-- Dzen box pretty config
data BoxPP = BoxPP
	{ bgColorBPP   :: String
	, fgColorBPP   :: String
	, boxColorBPP  :: String
	, leftIconBPP  :: String
	, rightIconBPP :: String
	, boxHeightBPP :: Int
	}

-- Dzen clickable area config
data CA = CA
	{ leftClickCA   :: String
	, middleClickCA :: String
	, rightClickCA  :: String
	, wheelUpCA     :: String
	, wheelDownCA   :: String
	}
 
-- Create a dzen string with its flags
dzenFlagsToStr :: DF -> String
dzenFlagsToStr df =
	" -x '" ++ (show $ xPosDF df) ++
	"' -y '" ++ (show $ yPosDF df) ++
	"' -w '" ++ (show $ widthDF df) ++
	"' -h '" ++ (show $ heightDF df) ++
	"' -ta '" ++ alignmentDF df ++
	"' -fg '" ++ fgColorDF df ++
	"' -bg '" ++ bgColorDF df ++
	"' -fn '" ++ fontDF df ++
	"' -e '" ++ eventDF df ++
	"' " ++ extrasDF df
 
-- Uses dzen format to draw a "box" arround a given text
dzenBoxStyle :: BoxPP -> String -> String
dzenBoxStyle bpp t =
	"^fg(" ++ (boxColorBPP bpp) ++
	")^i(" ++ (leftIconBPP bpp)  ++
	")^ib(1)^r(1920x" ++ (show $ boxHeightBPP bpp) ++
	")^p(-1920)^fg(" ++ (fgColorBPP bpp) ++
	")" ++ t ++
	"^fg(" ++ (boxColorBPP bpp) ++
	")^i(" ++ (rightIconBPP bpp) ++
	")^fg(" ++ (bgColorBPP bpp) ++
	")^r(1920x" ++ (show $ boxHeightBPP bpp) ++
	")^p(-1920)^fg()^ib(0)"
 
-- Uses dzen format to make dzen text clickable
dzenClickStyle :: CA -> String -> String
dzenClickStyle ca t = "^ca(1," ++ leftClickCA ca ++
	")^ca(2," ++ middleClickCA ca ++
	")^ca(3," ++ rightClickCA ca ++
	")^ca(4," ++ wheelUpCA ca ++
	")^ca(5," ++ wheelDownCA ca ++
	")" ++ t ++
	"^ca()^ca()^ca()^ca()^ca()"
 
-- Launch dzen through the system shell and return a Handle to its standard input
dzenSpawnPipe df = spawnPipe $ "dzen2" ++ dzenFlagsToStr df
 
-- Logger version of dzenBoxStyle
dzenBoxStyleL :: BoxPP -> Logger -> Logger
dzenBoxStyleL bpp l = (fmap . fmap) (dzenBoxStyle bpp) l
 
-- Logger version of dzenClickStyle
dzenClickStyleL :: CA -> Logger -> Logger
dzenClickStyleL ca l = (fmap . fmap) (dzenClickStyle ca) l

-- Gets the current resolution given a display and a screen
getScreenRes :: String -> Int -> IO Res
getScreenRes d n = do
	dpy <- openDisplay d
	r <- liftIO $ getScreenInfo dpy
	closeDisplay dpy
	return $ Res
		{ xRes = fromIntegral $ rect_width $ r !! n
		, yRes = fromIntegral $ rect_height $ r !! n
		}
 
-- Screen Resolution
data Res = Res
	{ xRes :: Int
	, yRes :: Int
	}
 
-- Resolution logger
screenRes :: String -> Int -> Logger
screenRes d n = do
	res <- liftIO $ getScreenRes d n
	return $ return $ (show $ xRes res) ++ "x" ++ (show $ yRes res)
