import           XMonad                          hiding ((|||))
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.WindowBringer           (gotoMenuArgs', bringMenuArgs')
import           XMonad.Config.Desktop                  (desktopConfig)
import           XMonad.Hooks.ManageDocks               (avoidStruts, ToggleStruts(..))
import           XMonad.Hooks.ManageHelpers             (doFullFloat, isFullscreen)
import           XMonad.Hooks.ManageHelpers             (isInProperty)
import           XMonad.Hooks.RefocusLast               (refocusLastLayoutHook)
import           XMonad.Hooks.SetWMName                 (setWMName)
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.DecorationMadness        (circleSimpleDefaultResizable)
import           XMonad.Layout.LayoutCombinators        ((|||))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ThreeColumns             (ThreeCol(..))
import           XMonad.Layout.TrackFloating
import           XMonad.Util.EZConfig                   (additionalKeysP)
import           XMonad.Layout.MultiToggle              (mkToggle, single, EOT(EOT), (??))
import           XMonad.Layout.MultiToggle.Instances    (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import           XMonad.Layout.Named
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.Tabbed

import qualified XMonad.DBus                     as D
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.StackSet                 as W

import           ResizableSpacing

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

myLayouts =
        tabbed
    ||| vertical
    ||| horizontal
    ||| threeColumn
    ||| circle
    where
        tabbed = named "tabbed" $ simpleTabbed
        vertical   = named "vertical" $ spacing 0 $ Tall 1 (3/100) (1/2)
        horizontal = named "horizontal" $ Mirror vertical
--        full = named "full" $ smartBorders $ spacing 0 $ Full
        threeColumn = named "3col" $ spacing 0 $ ThreeCol 1 (3/100) (1/3)
        circle = named "circle" $ circleSimpleDefaultResizable

layoutHook' =
  refocusLastLayoutHook . trackFloating -- floating window focus fix (e.g. intellij find window)
    $ avoidStruts
    $ smartBorders
    $ mkToggle (single NOBORDERS)
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myLayouts


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
    , ("M-C-S-\\", bringMenuArgs' "rofi" ["-dmenu", "-i", "-show", "combi"])

    , ("M-S-n", spawn "thunar")

    , ("M-b", sendMessage ToggleStruts)
    , ("M-S-b", sendMessage (MT.Toggle NOBORDERS)) --Toggle borders on/off
    , ("M-r", sendMessage (MT.Toggle REFLECTX)) --Invert master side in vertical layouts
    , ("M-S-r", sendMessage (MT.Toggle REFLECTY)) --Invert master side in horizontal layouts
    , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    , ("M-S-r", spawn "xmonad --restart && . /home/tim/.machineconf")
    , ("M-C-S-l", spawn "slock")

    , ("M-<F1>", sendMessage $ JumpToLayout "tabbed")
    , ("M-<F2>", sendMessage $ JumpToLayout "vertical")
    , ("M-<F3>", sendMessage $ JumpToLayout "horizontal")
    , ("M-<F4>", sendMessage $ JumpToLayout "3col")
    , ("M-<F5>", sendMessage $ JumpToLayout "circle")

    , ("M-<F10>", windows copyToAll)
    , ("M-<F12>", sendMessage $ IncSpacing 5)
    , ("M-<F11>", sendMessage $ DecSpacing 5)
    , ("M-S-<F10>", killAllOtherCopies)    -- remove from all but current

    , ("<Scroll_lock>", spawn "sps")
    , ("<Pause>", spawn "playerctl play-pause")
    ] ++
    -- move to workspace
    [ (otherModMasks ++ "M-" ++ key, action tag)
      | (tag, key)  <- zip myWorkspaces ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=" ]
      , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
      , ("S-", windows . W.shift)]
    ] ++

    -- switch to workspace
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

