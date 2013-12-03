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

import           XMonad.Actions.WindowBringer    (gotoMenu)
import           XMonad.Util.EZConfig            (additionalKeysP)
import           XMonad.Util.Loggers
import           XMonad.Util.Run                 (hPutStrLn, spawnPipe)

import           Control.Applicative             ((<$>))
import           Data.List                       (elemIndex, isPrefixOf)
import           Data.Maybe                      (fromMaybe)
import           Data.Ratio                      ((%))
import           Data.Traversable                (traverse)

import qualified XMonad.StackSet                 as W




modMask' = mod4Mask


main = do
  _ <- spawn "pgrep conky | xargs kill -9"
  workspaceBar <- spawnDzen myStatusBar
  _ <- spawnToDzen "conky -c /home/tim/.xmonad/conkytop" conkyBar
  _ <- spawn "conky -c /home/tim/.xmonad/conkydesktop"
  xmonad $ withUrgencyHook NoUrgencyHook $ xfceConfig {
    modMask              = modMask'
    , layoutHook         = layoutHook'
    , terminal           = "urxvt"
    , borderWidth        = 2
    , normalBorderColor  = "#cccccc"
    , manageHook         = manageHook' <+> manageHook xfceConfig
    , logHook            = logHook' workspaceBar
    , workspaces = workspaces'
    }  `additionalKeysP` keys'


wsNames = [ "1.code", "2.terminal", "3.web", "4.emacs", "5", "6.music", "7.chat", "8.mail", "9.im", "0.skype" ]

workspaces' = clickable . map dzenEscape $ wsNames
    where clickable l = [
                          "^ca(1,xdotool key super+" ++ show n ++ ")" ++ ws ++ "^ca()"
                          | (i,ws) <- zip [1..] l
                          , let n = i
                        ]

wsNamed :: String -> WorkspaceId
wsNamed w =
    workspaces' !! fromMaybe 666 (elemIndex w wsNames)




layoutHook' =
  avoidStruts $ smartBorders $ onWorkspace (wsNamed "9.im") imLayout $ standardLayouts
  where
    standardLayouts = Full ||| tiled ||| Mirror tiled ||| circle
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
      ("C-\\", spawn "dmenu_run")
    , ("C-S-\\", gotoMenu)
    , ("<F1>", sendMessage $ JumpToLayout "Full")
    , ("<F2>", sendMessage $ JumpToLayout "Tall")
    , ("<F3>", sendMessage $ JumpToLayout "Mirror Tall")
    , ("<F4>", sendMessage $ JumpToLayout "circle")
    ] ++
    [ (otherModMasks ++ "M-" ++ key, action tag)
      | (tag, key)  <- zip workspaces' (map show [1,2,3,4,5,6,7,8,9,0])
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
    , ppCurrent         = wrap (highlight "[ ") (highlight " ]")  <$> plain
    , ppHidden          = plain
    , ppTitle           = plain
    , ppExtras          = [ logNumWindows ]
    , ppHiddenNoWindows = dzenColor "#444444" "#000000"
    , ppUrgent          = highlight . dzenStrip
    , ppLayout          = dzenColor "#1874CD" "#000000"
    , ppWsSep           = "  "
    , ppSep             = " | "
    , ppOrder           = \(ws:l:title:num:_) -> [ws, "^ca(1,xdotool key super+space)" ++ l ++ "^ca()"  ++ " " ++ num, title]
}


logHook' h = dynamicLogWithPP $ pp' h

logNumWindows = withWindowSet $ \ws -> (return . Just . numWindows) (W.current ws)

numWindows :: W.Screen a b c d e -> String
numWindows screen = highlight . show $ length ((W.integrate' . W.stack . W.workspace) screen)

