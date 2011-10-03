--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Util.Run
import Control.Monad
import Data.Monoid
import System.Exit
import System.IO

import XMonad.Actions.WindowMenu
import XMonad.Actions.WindowGo
import XMonad.Actions.GridSelect

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.LayoutHints
import XMonad.Layout.Maximize

import XMonad.Util.EZConfig
import XMonad.Util.Paste

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

withScreen screen f = screenWorkspace screen >>= flip whenJust (windows . f)
myModMask       = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask, xK_r     ), spawn "gmrun")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    , ((0                 , xK_Print ), spawn "import /tmp/screen.jpg")
    , ((controlMask       , xK_Print ), spawn "import -window root /screen.jpg")
    , ((modm              , xK_o     ), windowMenu)
    , ((modm              , xK_s     ), spawnSelected defaultGSConfig [terminal conf, "firefox-bin", "emacs --daemon", "desmume", "VisualBoyAdvance "])
    , ((modm              , xK_i     ), spawn "xcalib -i -a")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myLayout = avoidStruts . layoutHints $ tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

myManageHook = composeAll
             [ className =? "Firefox" --> viewShift "1:web"
             , className =? "Emacs" --> viewShift "2:editor"
             , className =? "Gvim" --> viewShift "2:editor"
             , className =? "XTerm" --> viewShift "3:term"
             , className =? "Evince" --> viewShift "4:doc"
             , className =? "Apvlv" --> viewShift "4:doc"
             , className =? "Stardict" --> viewShift "5:misc"
             , className =? "Desmume" --> viewShift "6"
             , className =? "com-topcoder-client-contestApplet-runner-generic" --> viewShift "6"
             , className =? "MPlayer" --> doFloat
             , className =? "Gimp" --> doFloat
             ]
  where viewShift = doF . liftM2 (.) W.greedyView W.shift

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

customPP :: PP
customPP = defaultPP { 
     			    ppHidden = xmobarColor "#00FF00" ""
			  , ppCurrent = xmobarColor "#FF0000" "" . wrap "[" "]"
			  , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
                          , ppLayout = xmobarColor "#FF0000" ""
                          , ppTitle = xmobarColor "#00FF00" "" . shorten 80
                          , ppSep = "<fc=#0033FF> | </fc>"
                     }
 

------------------------------------------------------------------------

myKeys2 = [ 
            ("C-; c", myRunOrRaiseNext "xterm -e tmux" (className =? "XTerm"))
          , ("C-; C-C", runOrRaiseNext "xterm" (className =? "XTerm"))
          , ("C-; e", myRunOrRaiseNext "emacsclient -c" (className =? "Emacs"))
          , ("C-; v", runOrRaiseNext "gvim" (className =? "Gvim"))
          , ("C-; f", runOrRaiseNext "firefox-bin" (className =? "Firefox"))
          , ("C-; i", runOrRaiseNext "evince" (className =? "Evince"))
          , ("C-; a", runOrRaiseNext "apvlv" (className =? "Apvlv"))
          , ("C-; d", runOrRaiseNext "stardict" (className =? "Stardict"))
          , ("C-; C-;", pasteChar controlMask ';')
            
          , ("M-m",   withFocused (sendMessage . maximizeRestore))
          ]
          where myRunOrRaiseNext :: String -> Query Bool -> X ()
                myRunOrRaiseNext = raiseNextMaybe . unsafeSpawn

main = do
     xmobar <- spawnPipe "/usr/bin/xmobar"
     xmonad $ defaultConfig {
      -- simple stuff
        terminal           = "xterm",
        focusFollowsMouse  = True,
        borderWidth        = 1,
        modMask            = mod4Mask,
        numlockMask        = mod2Mask,
        workspaces         = ["1:web","2:editor","3:term","4:doc","5:misc","6","7","8","9"],
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "#ff0000",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = mempty,
        logHook            = myLogHook xmobar,
        startupHook        = spawn "~/bin/start-tiling"
    } `additionalKeysP` myKeys2
