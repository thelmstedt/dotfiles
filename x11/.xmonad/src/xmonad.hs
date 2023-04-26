import           ResizableSpacing

import           XMonad                          hiding ((|||))
import           XMonad.Config.Desktop           (desktopConfig)

import           XMonad.Layout.DecorationMadness (circleSimpleDefaultResizable)
import           XMonad.Layout.LayoutCombinators ((|||))
import           XMonad.Layout.Named             (named)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect           (reflectHoriz)
import           XMonad.Layout.ThreeColumns      (ThreeCol(..))

import           XMonad.Hooks.ManageDocks        (avoidStruts, ToggleStruts(..))
import           XMonad.Hooks.ManageHelpers      (doFullFloat, isFullscreen)
import           XMonad.Hooks.SetWMName          (setWMName)
import           XMonad.Hooks.StatusBar         -- add status bar such as xmobar
import           XMonad.Hooks.StatusBar.PP      -- configure status bar printing printing
import           XMonad.Hooks.ManageHelpers      (isInProperty)

import           XMonad.Actions.WindowBringer    (gotoMenuArgs')

import           XMonad.Util.EZConfig            (additionalKeysP)

import qualified XMonad.StackSet                 as W
import qualified XMonad.DBus                     as D

main :: IO ()
main = do
  dbus <- D.connect
  _ <- D.requestAccess dbus
  let myConfig = desktopConfig {
       modMask              = mod4Mask
       , layoutHook         = layoutHook'
       , terminal           = "alacritty"
       , borderWidth        = 1
       , focusedBorderColor = "#cd0000"
       , normalBorderColor  = "#000000"
       , startupHook        = myStartupHook
       , manageHook         = manageHook' <+> manageHook desktopConfig
       , workspaces = myWorkspaces
       }  `additionalKeysP` keys'
    in xmonad . withEasySB(polybarSB dbus) defToggleStrutsKey $ myConfig

myStartupHook = do
  setWMName "LG3D"
  spawn "$HOME/.config/polybar/launch.sh"

polybarSB dbusConnection = statusBarGeneric "polybar -r" lh
  where lh = dynamicLogWithPP $ myLogHook dbusConnection

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "+" ]

layoutHook' =
  avoidStruts $ myBorders $ standardLayouts
  where
    -- todo is this doing more than smartborders?
--    myBorders = lessBorders (Combine Difference Screen OnlyScreenFloat)
    myBorders = smartBorders
    standardLayouts = full ||| tiled ||| reflectTiled ||| Mirror tiled ||| threeColumn ||| circle
    full = smartBorders $ spacing 0 $ Full
    tiled   = spacing 0 $ Tall nmaster delta ratio
    reflectTiled = named "Reflect Tall" $ reflectHoriz $ tiled
    threeColumn = spacing 0 $ named "3col" $ ThreeCol 1 (3/100) (1/3)
    circle = named "circle" $ circleSimpleDefaultResizable
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2

manageHook' =
  composeAll
    [ moveC "Firefox" "3"
    , moveC "chromium" "3"
    , moveC "Emacs" "4"
    , floatC "Gnome-calculator"
    , ignoreC "wine"
    , ignoreC "qllauncher"
    , floatC "Steam"
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"        --> doIgnore
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"  --> doIgnore

    , (resource  =? "desktop_window")     --> doFloat
    , isFullscreen                        --> doFullFloat
--    , (className =? "jetbrains-idea") <&&> ("win" `isPrefixOf`) <$> title --> doIgnore
    ]
  where
    moveC c w = (className =? c) -->  doShift w
    ignoreC c = (className =? c) --> doIgnore
    floatC c = (className =? c) --> doFloat

keys' =
    [
      ("M-\\", spawn "rofi -show combi")
    , ("M-S-\\", gotoMenuArgs' "rofi" ["-dmenu", "-i", "-show", "combi"])

    , ("M-S-n", spawn "thunar")
    , ("M-b", sendMessage ToggleStruts)

    , ("M-S-r", spawn "xmonad-x86_64-linux --recompile && xmonad-x86_64-linux --restart && . /home/tim/.machineconf")
    , ("C-M-S-l", spawn "slock")

    , ("M-<F1>", sendMessage $ JumpToLayout "Full")
    , ("M-<F2>", sendMessage $ JumpToLayout "Tall")
    , ("M-<F3>", sendMessage $ JumpToLayout "Mirror Tall")
    , ("M-<F4>", sendMessage $ JumpToLayout "Reflect Tall")
    , ("M-<F5>", sendMessage $ JumpToLayout "3col")
    , ("M-<F6>", sendMessage $ JumpToLayout "circle")

    , ("M-<F11>", sendMessage $ IncSpacing 15)
    , ("M-<F12>", sendMessage $ DecSpacing 15)

    , ("M-<Pause>", spawn "pps")
    , ("<Pause>", spawn "pps")

    , ("M-<Print>", spawn "sps")
    , ("<Print>", spawn "sps")

    , ("M-<Scroll_lock>", spawn "sps")
    , ("<Scroll_lock>", spawn "sps")
    ] ++
    [ (otherModMasks ++ "M-" ++ key, action tag)
      | (tag, key)  <- zip myWorkspaces ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=" ]
      , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
      , ("S-", windows . W.shift)]
    ] ++
    [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
         | (key, scr)  <- zip "we" [0,1] -- 2 monitors
         , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]

myLogHook dbus = def
    {
    ppOutput = D.send dbus
    , ppCurrent = wrap ("%{F" ++ blue2 ++ "} ") "%{F-}"
    , ppVisible = wrap ("%{F" ++ blue ++ "} ") "%{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") "%{F-}"
    , ppHidden = wrap " " ""
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = shorten 100
    , ppLayout = wrap ("%{F" ++ blue2 ++ "}") "%{F-}"
} where
  red       = "#fb4934"
  blue      = "#83a598"
  blue2     = "#2266d0"


