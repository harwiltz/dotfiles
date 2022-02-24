import Data.List (intercalate)
import System.Exit (ExitCode(..))
import System.IO
import System.Process (runInteractiveCommand, getProcessExitCode)

import qualified Xmobar as Xmb
import XMonad
import XMonad.Actions.Volume (toggleMute, raiseVolume, lowerVolume)
import XMonad.Actions.Navigation2D
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Types (Direction1D(..))

nav2dconfig = navigation2D def
                           (xK_k, xK_h, xK_j, xK_l)
                           [(mod4Mask, windowGo), (mod4Mask .|. shiftMask, windowSwap)]
                           False

main = (putStrLn bar) >> spawnPipe bar >>= (xmonad . ewmh . nav2dconfig . xmonadConfig)

bar :: String
bar = intercalate " " $ "xmobar":barArgs
    where barArgs :: [String]
          barArgs = [ "-t", "\"%StdinReader%}{%wake% %alsa:default:Master% | %date% [%battery%]\""
                    , "-d"
                    , "-C", show [batteryCmd, dateCmd, volumeCmd, screenCmd]
                    , "-f", "\"xft: xos4 Terminus\""
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

screenCmd = unwords [ "Run Com"
                    , show "/home/harwiltz/scripts/xmobar-screen-wakeness"
                    , show [""]
                    , show "wake"
                    , show 10
                    ]
--screenCmd = "Run ScreenWakeness \"wake\""

data ScreenWakeness = ScreenWakeness String deriving (Read, Show)

instance Xmb.Exec ScreenWakeness where
  alias (ScreenWakeness s) = s
  start sw cb = do
    (hstdin, hstdout, hstderr, ph) <- runInteractiveCommand cmd
    hClose hstdin
    hClose hstderr
    hClose hstdout
    hSetBinaryMode hstdout False
    hSetBuffering hstdout LineBuffering
    forever ph
    where forever ph =
            do
              ec <- getProcessExitCode ph
              case ec of
                 Nothing -> forever ph
                 Just (ExitFailure _) -> cb "Enabled"
                 Just ExitSuccess -> cb "Disabled"
          cmd = unwords [ "xset q"
                        , "|"
                        , "grep DPMS"
                        , "|"
                        , "grep Disabled"
                        ]

xmonadConfig h = docks desktopConfig { terminal           = "termite"
                                     , modMask            = mod4Mask
                                     , borderWidth        = 2
                                     , normalBorderColor  = "#222222"
                                     , focusedBorderColor = "#FFFF00"
                                     , layoutHook         = avoidStruts $ layouts
                                     , workspaces         = myWorkspaces
                                     , logHook            = dynamicLogWithPP prettyPrint
                                     , manageHook         = className =? "gksqt" --> doFloat
                                     } `additionalKeys` keyMaps
    where prettyPrint :: PP
          prettyPrint = xmobarPP { ppCurrent = xmobarColor "#FFFF00" "" . pad
                                 , ppHiddenNoWindows = xmobarColor "#444444" "" . pad
                                 , ppHidden = pad
                                 , ppOutput = hPutStrLn h
                                 }
          layouts =
            smartBorders emptyBSP |||
            noBorders Full

myWorkspaces :: [WorkspaceId]
myWorkspaces = romanNumerals

romanNumerals :: [WorkspaceId]
romanNumerals = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"]

stars :: [WorkspaceId]
stars = take 10 $ repeat "*"

keyMaps = [ ((mod4Mask, xK_Return), spawn "kitty")
          , ((mod4Mask, xK_e), spawn "emacsclient -nc")
          , ((mod4Mask, xK_p), spawn dmenuCmd)
          , ((mod1Mask, xK_l), spawn "xscreensaver-command -lock")
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
          , ((mod4Mask .|. mod1Mask, xK_o), spawn "/home/harwiltz/scripts/screenon.sh")
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
