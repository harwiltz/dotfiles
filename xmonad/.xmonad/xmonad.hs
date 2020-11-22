import Data.List (intercalate)
import System.IO

import XMonad
import XMonad.Actions.Volume (toggleMute, raiseVolume, lowerVolume)
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

main = spawnPipe bar >>= (xmonad . xmonadConfig)

bar :: String
bar = intercalate " " $ "xmobar":barArgs
    where barArgs :: [String]
          barArgs = [ "-t", "\"%StdinReader%}{%alsa:default:Master% | %date% [%battery%]\""
                    , "-d"
                    , "-C", show [batteryCmd, dateCmd, volumeCmd]
                    , "-f", "\"xft: xos4 terminus\""
                    ]

batteryCmd :: String
batteryCmd = unwords ["Run Battery", show batteryOpts, "50"]
    where batteryOpts :: [String]
          batteryOpts = [ "--template", "<acstatus>"
                        , "--Low"     , "20"
                        , "--High"    , "90"
                        , "--low"     , "#BB0000"
                        , "--high"    , "#336633"
                        , "--normal"  , "darkorange"
                        , "--"
                        , "-o"        , "<left>%"
                        , "-O"        , "<left>%(<fc=#ffff00>AC</fc>)"
                        , "-i"        , "<fc=#008800>Charged</fc>"
                        ]

dateCmd :: String
dateCmd = unwords [ "Run Date"
                  , show "<fc=#9999bb>(%a) %b %_d %Y</fc> | <fc=#bbbbdd>%T</fc>"
                  , show "date", "10"
                  ]

volumeCmd :: String
volumeCmd = unwords [ "Run Alsa"
                    , show "default"
                    , show "Master"
                    , show volumeOpts
                    ]
    where volumeOpts :: [String]
          volumeOpts = [ "--template", "<fc=#444477>Vol</fc>: <volume>% <status>" ]

xmonadConfig h = docks desktopConfig { terminal           = "termite"
                                     , modMask            = mod4Mask
                                     , borderWidth        = 2
                                     , normalBorderColor  = "#222222"
                                     , focusedBorderColor = "#FFFF00"
                                     , layoutHook         = avoidStruts $ layoutHook defaultConfig
                                     , workspaces         = myWorkspaces
                                     , logHook            = dynamicLogWithPP prettyPrint
                                     } `additionalKeys` keyMaps
    where prettyPrint :: PP
          prettyPrint = xmobarPP { ppCurrent = xmobarColor "#FFFF00" "" . pad
                                 , ppHiddenNoWindows = xmobarColor "#444444" "" . pad
                                 , ppHidden = pad
                                 , ppOutput = hPutStrLn h
                                 }

myWorkspaces :: [WorkspaceId]
myWorkspaces = romanNumerals

romanNumerals :: [WorkspaceId]
romanNumerals = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"]

stars :: [WorkspaceId]
stars = take 10 $ repeat "*"

keyMaps = [ ((mod4Mask, xK_Return), spawn "termite")
          , ((mod4Mask, xK_e), spawn "emacs")
          , ((mod4Mask, xK_p), spawn dmenuCmd)
          ] ++ --additional workspace stuff
          [ ((mod4Mask, xK_0), windows $ W.greedyView $ myWorkspaces !! 9)
          , ((mod4Mask .|. shiftMask, xK_0), windows $ W.shift $ myWorkspaces !! 9)
          ] ++ -- volume stuff
          [ ((mod4Mask .|. mod1Mask, xK_equal), raiseVolume volDeltaPct >> return())
          , ((mod4Mask .|. mod1Mask, xK_minus), lowerVolume volDeltaPct >> return())
          , ((mod4Mask .|. mod1Mask, xK_m), toggleMute >> return())
          ]
    where volDeltaPct = 5.0

dmenuCmd = unwords [ "dmenu_run"
                   , "-fn", show "xos4 Terminus"
                   , "-p" , show "run:"
                   , "-nb", show "#111"
                   , "-nf", show "#888"
                   , "-sb", show "#333"
                   , "-sf", show "#ff0"
                   , "-b"
                   ]
