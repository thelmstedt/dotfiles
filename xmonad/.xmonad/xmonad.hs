import           Dzen

import           XMonad                          hiding ((|||))
import           XMonad.Config.Xfce              (xfceConfig)

import           XMonad.Layout.DecorationMadness (circleSimpleDefaultResizable)
import           XMonad.Layout.IM                (Property (..), withIM)
import           XMonad.Layout.LayoutCombinators (JumpToLayout (..), (|||))
import           XMonad.Layout.Named             (named)
import           XMonad.Layout.NoBorders         (smartBorders)
import           XMonad.Layout.PerWorkspace      (onWorkspace)
import           XMonad.Layout.Reflect           (reflectHoriz)

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks        (avoidStruts)
import           XMonad.Hooks.ManageHelpers      (doFullFloat, isFullscreen)
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.SetWMName

import           XMonad.Actions.WindowBringer    (gotoMenu)
import           XMonad.Util.EZConfig            (additionalKeysP)
import           XMonad.Util.Loggers
import           XMonad.Util.Run                 (hPutStrLn)

import           Data.List                       (elemIndex, isPrefixOf)
import           Data.Maybe                      (fromMaybe)
import           Data.Ratio                      ((%))

import qualified XMonad.StackSet                 as W

import ResizableSpacing 

modMask' = mod4Mask


main = do
  spawnToDzen "/home/tim/.xmonad/startup.sh" conkyBar
  
  workspaceBar <- spawnDzen myStatusBar
  xmonad $ withUrgencyHook NoUrgencyHook $ xfceConfig {
    modMask              = modMask'
    , layoutHook         = layoutHook'
    , terminal           = "urxvt"
    , borderWidth        = 2
    , normalBorderColor  = "#cccccc"
    , startupHook        = setWMName "LG3D"
    , manageHook         = manageHook' <+> manageHook xfceConfig
    , logHook            = logHook' workspaceBar
    , workspaces = myWorkspaces
    }  `additionalKeysP` keys'


myWorkspaces = [ "1.code", "2.terminal", "3.web", "4.emacs", "5", "6.music", "7.chat", "8.mail", "9.im", "0.skype" ]

wsNamed :: String -> WorkspaceId
wsNamed w =
    myWorkspaces !! fromMaybe 666 (elemIndex w myWorkspaces)

layoutHook' = 
  avoidStruts $ smartBorders $ onWorkspace (wsNamed "9.im") imLayout $ standardLayouts
  where
    standardLayouts = spacing 0 $ Full ||| tiled ||| Mirror tiled ||| circle
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100
    circle = named "circle" $ avoidStruts circleSimpleDefaultResizable
    imLayout =  named "im" $ avoidStruts $ reflectHoriz
                $ withIM (1%9) pidginRoster standardLayouts
    pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"


manageHook' =
  composeAll
    [ moveC "jetbrains-idea" "1.code"
    , moveC "Firefox" "3.web"
    , moveC "Emacs" "4.emacs"
    , moveC "Hipchat" "7.chat"
    , moveC "Evolution" "8.mail"
    , moveC "Pidgin" "9.im"
    , ignoreC "vlc"
    , ignoreC "wine"
    , floatC "Steam"
    , (resource  =? "desktop_window")     --> doFloat
    , isFullscreen                        --> doFullFloat
    , (className =? "jetbrains-idea") <&&> ("win" `isPrefixOf`) <$> title --> doIgnore
    ]
  where
    moveC c w = (className =? c) -->  doF (W.shift $ wsNamed w)
    ignoreC c = (className =? c) --> doIgnore
    floatC c = (className =? c) --> doFloat



keys' =
    [
      ("C-\\", spawn "exe=`/home/tim/.xmonad/bin/dmenu-with-yeganesh` && eval \"exec $exe\"")
    , ("C-S-\\", gotoMenu)
    , ("M-<F1>", sendMessage $ JumpToLayout "Full")
    , ("M-<F2>", sendMessage $ JumpToLayout "Tall")
    , ("M-<F3>", sendMessage $ JumpToLayout "Mirror Tall")
    , ("M-<F4>", sendMessage $ JumpToLayout "circle")
    , ("M-<F5>", sendMessage $ IncSpacing 10)
    , ("M-<F6>", sendMessage $ DecSpacing 10)
    , ("M-r", spawn "xmonad --recompile && xmonad --restart")
    , ("C-M1-<Backspace>", spawn "xfce4-session-logout")
    ] ++
    [ (otherModMasks ++ "M-" ++ key, action tag)
      | (tag, key)  <- zip myWorkspaces (map show [1,2,3,4,5,6,7,8,9,0])
      , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
      , ("S-", windows . W.shift)]
    ]



myStatusBar = DzenConf {
      x_position = Just 0
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
    , width      = Just 720
    , height     = Just 24
    , alignment  = Just RightAlign
    , font       = Just "Bitstream Sans Vera:pixelsize=13"
    , fg_color   = Just "#ffffff"
    , bg_color   = Just "#000000"
    , exec       = []
    , addargs    = []
}



highlight = dzenColor "#ebac54" "#000000"
plain = dzenColor "#e5e5e5" "#000000"

pp' h = dzenPP {
      ppOutput          = hPutStrLn h
    , ppCurrent         = (wrap (highlight "[ ") (highlight " ]")  <$> plain) . clickable myWorkspaces
    , ppHidden          = plain . clickable myWorkspaces
    , ppHiddenNoWindows = dzenColor "#444444" "#000000" . clickable myWorkspaces
    , ppUrgent          = highlight . dzenStrip
    , ppTitle           = plain
    , ppExtras          = [ logNumWindows  ] -- 4th index onwards from [] arg to ppOrder
    , ppLayout          = dzenColor "#1874CD" "#000000"
    , ppWsSep           = " "
    , ppSep             = " | "
    , ppOrder           = \(ws:l:title:num:_) -> [ws, "^ca(1,xdotool key super+Tab)" ++ l ++ "^ca()"  ++ " " ++ num, title]
}


logHook' h = dynamicLogWithPP $ pp' h

logNumWindows :: X (Maybe String)
logNumWindows = withWindowSet $ \ws -> (return . Just . numWindows) (W.current ws)

numWindows :: W.Screen a b c d e -> String
numWindows screen = highlight . show $ length ((W.integrate' . W.stack . W.workspace) screen)

-- Wraps a workspace name with a dzen clickable action that focusses that workspace
clickable workspaces workspace = clickableExp workspaces 1 workspace
 
clickableExp [] _ ws = ws
clickableExp (ws:other) n l | l == ws = "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()"
                            | otherwise = clickableExp other (n+1) l
