{-# OPTIONS -fno-warn-missing-signatures #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig

myConfig = defaultConfig { modMask = mod4Mask, terminal = "xfce4-terminal" }
    `additionalKeysP`
        [ ("M-f", spawn "nautilus --no-desktop ~/Downloads")
        , ("M-S-q", spawn "gnome-session-quit --no-prompt")
        , ("M-S-,", spawn "gnome-control-center") ]

main = xmonad =<< xmobar myConfig
