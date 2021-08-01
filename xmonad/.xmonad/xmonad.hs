import Data.List (intercalate)
import System.IO

import XMonad
import XMonad.Actions.Volume (toggleMute, raiseVolume, lowerVolume)
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Grid
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Types (Direction1D(..))

main = spawnPipe bar >>= (xmonad . ewmh . xmonadConfig)

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
                                     , layoutHook         = avoidStruts $ layouts
                                     , workspaces         = myWorkspaces
                                     , logHook            = dynamicLogWithPP prettyPrint
                                     } `additionalKeys` keyMaps
    where prettyPrint :: PP
          prettyPrint = xmobarPP { ppCurrent = xmobarColor "#FFFF00" "" . pad
                                 , ppHiddenNoWindows = xmobarColor "#444444" "" . pad
                                 , ppHidden = pad
                                 , ppOutput = hPutStrLn h
                                 }
          layouts =
            Tall 1 (3/100) (1/2) |||
            emptyBSP |||
            Mirror (Tall 1 (3/100) (3/5)) ||| Full

myWorkspaces :: [WorkspaceId]
myWorkspaces = romanNumerals

romanNumerals :: [WorkspaceId]
romanNumerals = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"]

stars :: [WorkspaceId]
stars = take 10 $ repeat "*"

keyMaps = [ ((mod4Mask, xK_Return), spawn "termite")
          , ((mod4Mask, xK_e), spawn "emacsclient -nc")
          , ((mod4Mask, xK_p), spawn dmenuCmd)
          ] ++ --additional workspace stuff
          [ ((mod4Mask, xK_0), windows $ W.greedyView $ myWorkspaces !! 9)
          , ((mod4Mask .|. shiftMask, xK_0), windows $ W.shift $ myWorkspaces !! 9)
          ] ++ -- volume stuff
          [ ((mod4Mask .|. mod1Mask, xK_equal), raiseVolume volDeltaPct >> return())
          , ((mod4Mask .|. mod1Mask, xK_minus), lowerVolume volDeltaPct >> return())
          , ((mod4Mask .|. mod1Mask, xK_m), toggleMute >> return())
          ] ++ -- brightness stuff
          [ ((mod4Mask .|. controlMask, xK_equal), spawn "xbacklight -inc 10" )
          , ((mod4Mask .|. controlMask, xK_minus), spawn "xbacklight -dec 10" )
          ] ++ --bspc stuff
          [ ((mod4Mask .|. mod1Mask, xK_l), sendMessage $ ExpandTowards R)
          , ((mod4Mask .|. mod1Mask, xK_h), sendMessage $ ExpandTowards L)
          , ((mod4Mask .|. mod1Mask, xK_j), sendMessage $ ExpandTowards D)
          , ((mod4Mask .|. mod1Mask, xK_k), sendMessage $ ExpandTowards U)
          , ((mod4Mask, xK_r), sendMessage Rotate)
          , ((mod4Mask, xK_s), sendMessage Swap)
          , ((mod4Mask, xK_n), sendMessage FocusParent)
          , ((mod4Mask .|. controlMask, xK_n), sendMessage SelectNode)
          , ((mod4Mask .|. shiftMask, xK_n), sendMessage MoveNode)
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
