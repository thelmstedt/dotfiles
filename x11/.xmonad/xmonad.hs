import           ResizableSpacing

import           XMonad                          hiding ((|||))
import           XMonad.Config.Desktop           (desktopConfig)

import           XMonad.Layout.DecorationMadness (circleSimpleDefaultResizable)
import           XMonad.Layout.IM                (Property (..), withIM)
import           XMonad.Layout.LayoutCombinators (JumpToLayout (..), (|||))
import           XMonad.Layout.Named             (named)
import           XMonad.Layout.NoBorders         (smartBorders, noBorders)
import           XMonad.Layout.PerWorkspace      (onWorkspace)
import           XMonad.Layout.Reflect           (reflectHoriz)
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.Gaps

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers      (doFullFloat, isFullscreen)
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.ManageHelpers

import           XMonad.Actions.WindowBringer    (gotoMenuArgs')
import           XMonad.Util.EZConfig            (additionalKeysP)

import           XMonad.Util.Run                 (hPutStrLn)


import           Data.List                       (elemIndex, isPrefixOf)
import           Data.Ratio                      ((%))

import           XMonad.Actions.WindowGo         (runOrRaise)

import qualified XMonad.StackSet                 as W

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad $ withUrgencyHook NoUrgencyHook $ desktopConfig {
    modMask              = mod4Mask
    , layoutHook         = layoutHook'
    , terminal           = "alacritty"
    , borderWidth        = 1
    , focusedBorderColor = "#cd0000"
    , normalBorderColor  = "#cccccc"
    , startupHook        = myStartupHook
    , manageHook         = manageHook' <+> manageHook desktopConfig
    , logHook            = dynamicLogWithPP (myLogHook dbus)
    , workspaces = myWorkspaces
    }  `additionalKeysP` keys'

myStartupHook = do
  setWMName "LG3D"
  spawn "$HOME/.config/polybar/launch.sh"

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "+" ]

layoutHook' =
  avoidStruts $ smartBorders $ onWorkspace "9" imLayout $ standardLayouts
  where
    standardLayouts = (spacing 0 $ Full) ||| tiled ||| Mirror tiled ||| threeColumn ||| circle
    tiled   = spacing 0 $ Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100
    threeColumn = named "3col" $ ThreeCol 1 (3/100) (1/3)
    circle = named "circle" $ avoidStruts circleSimpleDefaultResizable
    imLayout = named "im" $ avoidStruts $ reflectHoriz $ withIM (1%9) pidginRoster standardLayouts
    pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"


manageHook' =
  composeAll
    [ moveC "Firefox" "3"
    , moveC "chromium" "3"
    , moveC "Emacs" "4"
    , moveC "Slack" "7"
    , moveC "Pidgin" "9"
    , ignoreC "vlc"
    , floatC "VirtualBox"
    , ignoreC "wine"
    , ignoreC "qllauncher"
    --, ignoreC "sun-awt-X11-XDialogPeer"
    , floatC "insync.py"
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

    , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")

    , ("M-<F1>", sendMessage $ JumpToLayout "Full")
    , ("M-<F2>", sendMessage $ JumpToLayout "Tall")
    , ("M-<F3>", sendMessage $ JumpToLayout "Mirror Tall")
    , ("M-<F4>", sendMessage $ JumpToLayout "3col")
    , ("M-<F5>", sendMessage $ JumpToLayout "circle")

    , ("M-<F11>", sendMessage $ IncSpacing 5)
    , ("M-<F12>", sendMessage $ DecSpacing 5)
    ] ++
    [ (otherModMasks ++ "M-" ++ key, action tag)
      | (tag, key)  <- zip myWorkspaces ((map show [1,2,3,4,5,6,7,8,9,0]) ++ ["-", "="])
      , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
      , ("S-", windows . W.shift)]
    ] ++
    [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
--         | (key, scr)  <- zip "qwe" [0,1,2] -- specifically work triple monitors
         | (key, scr)  <- zip "we" [0,1] -- 2 monitors
         , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]

fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#505050"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
white     = "#eeeeee"

pur2      = "#5b51c9"
blue2     = "#2266d0"


myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ blue2 ++ "} ") "%{F-}"
    , ppVisible = wrap ("%{F" ++ blue ++ "} ") "%{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") "%{F-}"
    , ppHidden = wrap " " ""
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = myAddSpaces 80
    , ppLayout = wrap ("%{F" ++ blue2 ++ "}") "%{F-}"
}

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str
