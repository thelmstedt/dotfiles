import           Dzen

import           XMonad                          hiding ((|||))
import           XMonad.Config.Xfce              (xfceConfig)

import           XMonad.Layout.DecorationMadness (circleSimpleDefaultResizable)
import           XMonad.Layout.IM                (Property (..), withIM)
import           XMonad.Layout.Named             (named)
import           XMonad.Layout.NoBorders         (smartBorders)
import           XMonad.Layout.PerWorkspace      (onWorkspaces)
import           XMonad.Layout.Reflect           (reflectHoriz)

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks        (avoidStruts)
import           XMonad.Hooks.ManageHelpers      (doFullFloat, isFullscreen)

import           XMonad.Util.EZConfig            (additionalKeysP)
import           XMonad.Util.Run                 (hPutStrLn, spawnPipe)

import           XMonad.Actions.WindowBringer    (gotoMenu)

import           Data.List                       (isPrefixOf)
import           Data.Ratio                      ((%))

import           Control.Applicative             ((<$>))

import           XMonad.Layout.LayoutCombinators (JumpToLayout (..), (|||))
import qualified XMonad.StackSet                 as W

import           XMonad.Hooks.UrgencyHook


modMask' = mod4Mask


main = do
  workspaceBar <- spawnDzen myStatusBar
  conky <- spawnToDzen "conky -c /home/tim/.xmonad/conkytop" conkyBar

  xmonad $ withUrgencyHook NoUrgencyHook $ xfceConfig {
    modMask              = modMask'
    , layoutHook         = avoidStruts layoutHook'
    , terminal           = "urxvt"
    , borderWidth        = 2
    , normalBorderColor  = "#cccccc"
    , manageHook         = manageHook' <+> manageHook xfceConfig
    , logHook            = myLogHook workspaceBar
    , workspaces = workspaces'
    }  `additionalKeysP` keys'




layoutHook' =
  smartBorders
  $ onWorkspaces ["9.im"] imLayout
  $ standardLayouts
  where
    standardLayouts = Full ||| tiled ||| Mirror tiled ||| circle
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100
    circle = named "circle" $ avoidStruts circleSimpleDefaultResizable
    imLayout =  named "im" $ avoidStruts $ reflectHoriz
                $ withIM (1%9) pidginRoster
                $ withIM (1%8) skypeRoster standardLayouts
    pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"
    skypeRoster = ClassName "Skype" `And` Title "tim.helmstedt - Skype"



manageHook' =
  composeAll
    [ moveC "jetbrains-idea" "1.code"
    , moveC "Firefox" "3.web"
    , moveC "Emacs" "4.emacs"
    , moveC "Hipchat" "7.hipchat"
    , moveC "Evolution" "8.mail"
    , moveC "Pidgin" "9.im"
    , ignoreC "vlc"
    , (resource  =? "desktop_window")     --> doFloat
    , isFullscreen                        --> doFullFloat
    , (className =? "jetbrains-idea") <&&> ("win" `isPrefixOf`) <$> title --> doIgnore
    ]
  where
    moveC c w = (className =? c) --> doShift w
    ignoreC c = (className =? c) --> doIgnore
    floatC c = (className =? c) --> doFloat



workspaces' :: [String]
workspaces' =
  [ "1.code", "2.terminal", "3.web", "4.emacs", "5", "6.music", "7.chat", "8.mail", "9.im", "0.skype" ]


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
    , y_position = Just (-1)
    , width      = Just 1200
    , height     = Just 24
    , alignment  = Just LeftAlign
    , font       = Just "Bitstream Sans Vera:pixelsize=13"
    , fg_color   = Just "#ffffff"
    , bg_color   = Just "#000000"
    , exec       = []
    , addargs    = []
}


conkyBar = DzenConf {
      x_position = Just 1200
    , y_position = Just (-1)
    , width      = Just 720
    , height     = Just 24
    , alignment  = Just RightAlign
    , font       = Just "Bitstream Sans Vera:pixelsize=13"
    , fg_color   = Just "#ffffff"
    , bg_color   = Just "#000000"
    , exec       = []
    , addargs    = []
}

myLogHook h = dynamicLogWithPP $ myPrettyPrinter h


highlight = dzenColor "#D43D1A" "#000000"
plain = dzenColor "#e5e5e5" "#000000"

-- Pretty printer for dzen workspace bar
myPrettyPrinter h = dzenPP {
      ppOutput          = hPutStrLn h
    , ppCurrent         =  wrap (highlight "[ ") (highlight " ]")  <$> plain
    , ppHidden          = plain
    , ppTitle           = plain
    , ppHiddenNoWindows = dzenColor "#444444" "#000000"
    , ppUrgent          = highlight . dzenStrip
    , ppLayout          = dzenColor "#465E84" "#000000"
    , ppWsSep           = "  "
    , ppSep             = " | "
}

