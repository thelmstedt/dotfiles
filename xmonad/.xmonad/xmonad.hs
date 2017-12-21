import           Dzen
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
import           XMonad.Hooks.ManageDocks        (avoidStruts)
import           XMonad.Hooks.ManageHelpers      (doFullFloat, isFullscreen)
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook

import           XMonad.Actions.WindowBringer    (gotoMenuArgs)
import           XMonad.Util.EZConfig            (additionalKeysP)

import           XMonad.Util.Run                 (hPutStrLn)


import           Data.List                       (elemIndex, isPrefixOf)
import           Data.Ratio                      ((%))

import           XMonad.Actions.WindowGo         (runOrRaise)

import qualified XMonad.StackSet                 as W

main :: IO ()
main = do
  spawnToDzen "/home/tim/.xmonad/bin/startup.sh" conkyBar

  workspaceBar <- spawnDzen myStatusBar
  xmonad $ withUrgencyHook NoUrgencyHook $ desktopConfig {
    modMask              = mod4Mask
    , layoutHook         = layoutHook'
    , terminal           = "urxvt"
    , borderWidth        = 1
    , focusedBorderColor = "#cd0000"
    , normalBorderColor  = "#cccccc"
    , startupHook        = setWMName "LG3D"
    , manageHook         = manageHook' <+> manageHook desktopConfig
    , logHook            = setWMName "LG3D" >> dynamicLogWithPP (pp' workspaceBar)
    , workspaces = myWorkspaces
    }  `additionalKeysP` keys'


myWorkspaces = [ "1.code", "2.terminal", "3.web", "4.emacs", "5", "6.music", "7.chat", "8", "9.im", "10", "11", "12" ]

layoutHook' =
  barGap $ avoidStruts $ smartBorders $ onWorkspace "9.im" imLayout $ standardLayouts
  where
    barGap = gaps [(U, 24)]
    standardLayouts = (spacing 0 $ Full) ||| tiled ||| Mirror tiled ||| threeColumn ||| circle
    tiled   = spacing 5 $ Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100
    threeColumn = named "3col" $ ThreeCol 1 (3/100) (1/3)
    circle = named "circle" $ avoidStruts circleSimpleDefaultResizable
    imLayout = named "im" $ avoidStruts $ reflectHoriz $ withIM (1%9) pidginRoster standardLayouts
    pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"


manageHook' =
  composeAll
    [ moveC "jetbrains-idea" "1.code"
    , moveC "Firefox" "3.web"
    , moveC "chromium" "3.web"
    , moveC "Emacs" "4.emacs"
    , moveC "hipchat" "7.chat"
    , moveC "Slack" "7.chat"
    , moveC "Thunderbird" "8.mail"
    , moveC "Pidgin" "9.im"
    , ignoreC "vlc"
    , floatC "VirtualBox"
    , ignoreC "wine"
    , ignoreC "qllauncher"
    --, ignoreC "sun-awt-X11-XDialogPeer"
    , floatC "insync.py"
    , floatC "Steam"
    , (resource  =? "desktop_window")     --> doFloat
    , isFullscreen                        --> doFullFloat
    -- , (className =? "jetbrains-idea") <&&> ("win" `isPrefixOf`) <$> title --> doIgnore
    ]
  where
    moveC c w = (className =? c) -->  doShift w
    ignoreC c = (className =? c) --> doIgnore
    floatC c = (className =? c) --> doFloat

myDzenFontLarge = ["-fn", "-*-*-*-*-*-18-*-*-*-*-*-*-*"]
myDzenColorsSolarized = ["-nb","#002b36","-nf","#839496","-sb","#586e75","-sf","#002b36"]
myDzenGoto = ["-p","Go to window:"] ++ myDzenColorsSolarized ++ myDzenFontLarge

keys' =
    [
      ("M-\\", spawn "exe=`/home/tim/.xmonad/bin/dmenu-with-yeganesh` && eval \"exec $exe\"")
    , ("M-S-\\", gotoMenuArgs myDzenGoto)

    , ("M-S-n", spawn "thunar")

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
         | (key, scr)  <- zip "we" [1,0] -- 2 monitors
         , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]

myStatusBar = DzenConf {
      x_position = Just 0 -- should be 1920, but this forces bar across left hand monitor
    , y_position = Just 0
    , width      = Just 1000
    , height     = Just 24
    , alignment  = Just LeftAlign
    , font       = Just "Bitstream Sans Vera:pixelsize=13"
    , fg_color   = Just "#ffffff"
    , bg_color   = Just "#000000"
    , exec       = []
    , addargs    = []
}

conkyBar = DzenConf {
      x_position = Just 1000
    , y_position = Just 0
    , width      = Just 710
    , height     = Just 24
    , alignment  = Just RightAlign
    , font       = Just "Bitstream Sans Vera:pixelsize=13"
    , fg_color   = Just "#ffffff"
    , bg_color   = Just "#000000"
    , exec       = []
    , addargs    = []
}


pp' h = dzenPP {
      ppOutput          = hPutStrLn h
    , ppCurrent         = (wrap (highlight "[ ") (highlight " ]")  <$> blue) . clickable myWorkspaces
    , ppVisible         = (wrap (highlight "[ ") (highlight " ]")  <$> plain) . clickable myWorkspaces
    , ppHidden          = plain . clickable myWorkspaces
    , ppHiddenNoWindows = grey . clickable myWorkspaces
    , ppUrgent          = (wrap (red "[ ") (red " ]") <$> plain) . clickable myWorkspaces
    , ppTitle           = plain . wrapClickable "super+Tab" . dzenEscape
    , ppExtras          = [ logNumWindows  ] -- 4th index onwards from [] arg to ppOrder
    , ppLayout          = blue
    , ppWsSep           = " "
    , ppSep             = " | "
    , ppOrder           = \(ws:layout:title:num:_) ->
                           [ws, wrapClickable "super+space" (layout ++ " " ++ num), title ]
}
  where
    highlight = dzenColor "#ebac54" "#000000"
    plain = dzenColor "#e5e5e5" "#000000"
    red = dzenColor "#CD0000" "#000000"
    blue = dzenColor "#1874CD" "#000000"
    grey = dzenColor "#444444" "#000000"

    logNumWindows :: X (Maybe String)
    logNumWindows = withWindowSet $ \ws -> (return . Just . numWindows) (W.current ws)

    numWindows :: W.Screen a b c d e -> String
    numWindows screen = highlight . show $ length ((W.integrate' . W.stack . W.workspace) screen)

    -- Wraps a workspace name with a dzen clickable action that focuses that workspace
    clickable :: [String] -> String -> String
    clickable workspaces x =
      case elemIndex x workspaces of
        Nothing -> x
        Just n -> let key = "super+" ++ show (fudge n + 1)
                  in wrapClickable key x
      where
        fudge 10 = 0 --10th is 0 key
        fudge n = n

    wrapClickable :: String -> String -> String
    wrapClickable k x =
     "^ca(1, xdotool key " ++ k ++ ")" ++ x ++ "^ca()"
