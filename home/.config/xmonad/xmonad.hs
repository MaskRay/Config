{-# LANGUAGE
    TypeSynonymInstances,
    MultiParamTypeClasses,
    DeriveDataTypeable
    #-}

import Control.Monad
import Codec.Binary.UTF8.String (encodeString)
import Data.List
import qualified Data.Map as M
import Data.Function
import System.Exit
import System.IO
import System.Posix.Process (executeFile)
import System.Posix.Types (ProcessID)

import XMonad hiding ((|||))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.Commands
import XMonad.Actions.CopyWindow (copy, copyToAll, killAllOtherCopies, wsContainingCopies)
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects (Project(..), dynamicProjects, shiftToProjectPrompt, switchProjectPrompt)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation
import XMonad.Actions.MessageFeedback (tryMessage_)
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote (promote)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.SpawnOn (spawnOn)
--import XMonad.Actions.TopicSpace
import XMonad.Actions.Warp
import XMonad.Actions.WithAll (sinkAll, killAll)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty (dynamicTitle)  -- 0.12 broken; works with github version
--import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.FadeWindows
{-import EwmhDesktops hiding (fullscreenEventHook)-}
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Drawer
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName (SWNConfig(..), showWName')
import XMonad.Layout.Simplest (Simplest(Simplest))
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

{-
 - TABBED
 -}

-- myTabTheme =
--     def
--     { activeColor         = "black"
--     , inactiveColor       = "black"
--     , urgentColor         = "yellow"
--     , activeBorderColor   = "orange"
--     , inactiveBorderColor = "#333333"
--     , urgentBorderColor   = "black"
--     , activeTextColor     = "orange"
--     , inactiveTextColor   = "#666666"
--     , decoHeight          = 24
--     , fontName = "xft:Dejavu Sans Mono:size=14"
--     }

data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
     transform _ x k = k (renamed [Replace "TABBED"] (tabbedAlways shrinkText myTabTheme)) (const x)

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green       = "#859900"

-- sizes
gap         = 10
topbar      = 10
border      = 0
status      = 20

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02

myFont      = "-*-terminus-medium-*-*-*-*-160-*-*-*-*-*-*"
myBigFont   = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"

-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

myPromptTheme = def
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = 20
    , position              = Top
    }

warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Top
    }

hotPromptTheme = myPromptTheme
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
    }

myShowWNameTheme = def
    { swn_font              = myWideFont
    , swn_fade              = 0.5
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
    }


-- gimpLayout = named "Gimp" $ withIM (0.130) (Role "gimp-toolbox") $ (simpleDrawer 0.2 0.2 (Role "gimp-dock") `onRight` Full)
myLayout = showWorkspaceName
             $ onWorkspace "float" floatWorkSpace
             $ fullscreenFloat -- fixes floating windows going full screen, while retaining "bounded" fullscreen
             $ fullScreenToggle
             $ mirrorToggle
             $ reflectToggle
             $ flex ||| tabs
  where

--    testTall = Tall 1 (1/50) (2/3)
--    myTall = subLayout [] Simplest $ trackFloating (Tall 1 (1/20) (1/2))

    floatWorkSpace      = simplestFloat
    fullScreenToggle    = mkToggle (single FULL)
    mirrorToggle        = mkToggle (single MIRROR)
    reflectToggle       = mkToggle (single REFLECTX)
    smallMonResWidth    = 1920
    showWorkspaceName   = showWName' myShowWNameTheme

    named n             = renamed [(XMonad.Layout.Renamed.Replace n)]
    trimNamed w n       = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                                   (XMonad.Layout.Renamed.PrependWords n)]
    suffixed n          = renamed [(XMonad.Layout.Renamed.AppendWords n)]
    trimSuffixed w n    = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                                   (XMonad.Layout.Renamed.AppendWords n)]

    addTopBar           = noFrillsDeco shrinkText topBarTheme

    sGap                = quot gap 2
    myGaps              = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
    mySmallGaps         = gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]
    myBigGaps           = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]

    --------------------------------------------------------------------------
    -- Tabs Layout                                                          --
    --------------------------------------------------------------------------

    threeCol = named "Unflexed"
         $ avoidStruts
         $ addTopBar
         $ ThreeColMid 1 (1/10) (1/2)

    tabs = named "Tabs"
         $ avoidStruts
         $ addTopBar
         $ addTabs shrinkText myTabTheme
         $ Simplest


    flex = trimNamed 5 "Flex"
              $ avoidStruts
              -- don't forget: even though we are using X.A.Navigation2D
              -- we need windowNavigation for merging to sublayouts
              $ windowNavigation
              $ addTopBar
              $ addTabs shrinkText myTabTheme
              -- $ subLayout [] (Simplest ||| (mySpacing $ Accordion))
              $ subLayout [] (Simplest ||| Accordion)
              $ ifWider smallMonResWidth wideLayouts standardLayouts
              where
                  wideLayouts =
                        (suffixed "Wide 3Col" $ ThreeColMid 1 (1/20) (1/2))
                    ||| (trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP)
                  --  ||| fullTabs
                  standardLayouts =
                        (suffixed "Std 2/3" $ ResizableTall 1 (1/20) (2/3) [])
                    ||| (suffixed "Std 1/2" $ ResizableTall 1 (1/20) (1/2) [])


myBrowserClass      = "Chromium"
hangoutsResource    = "crx_nckgahadagoaajjgafhacjanaoiihapd"
isHangoutsFor s     = (className =? myBrowserClass
                      <&&> fmap (isPrefixOf "Google Hangouts") title
                      <&&> fmap (isInfixOf s) title)

-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

doSPFloat = customFloating $ W.RationalRect (1/7) (1/7) (5/7) (5/7)
myManageHook = composeAll $
    [ isDialog <&&> className =? myBrowserClass--> forceCenterFloat ] ++
    [ className =? c --> viewShift "web" | c <- ["Firefox"] ] ++
    [ className =? c <&&> role =? "browser" --> viewShift "web" | c <- ["Google-chrome", "Chrome", "Chromium"] ] ++
    [ resource =? hangoutsResource --> insertPosition End Newer ] ++
    -- [ role =? r --> doFloat | r <- ["pop-up", "app"]] ++ -- chrome has pop-up windows
    [ title =? "weechat" --> viewShift "im"] ++
    [ title =? "mutt" --> viewShift "mail"] ++
    [ className =? c --> viewShift "gimp" | c <- ["Gimp"] ] ++
    [ prefixTitle "emacs" --> doShift "emacs" ] ++
    [ className =? "Synapse" --> doIgnore ] ++
    [ manageDocks , namedScratchpadManageHook scratchpads ] ++
    [ className =? c --> ask >>= \w -> liftX (hide w) >> idHook | c <- ["XClipboard"] ] ++
    [ myCenterFloats --> doCenterFloat ] ++
    [ pure True --> tileBelow]
  where
    --isBrowserDialog = 
    role = stringProperty "WM_WINDOW_ROLE"
    prefixTitle prefix = fmap (prefix `isPrefixOf`) title
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    myCenterFloats = foldr1 (<||>)
        [ className =? "feh"
        , className =? "Display"
        , isDialog
        ]
    mySPFloats = foldr1 (<||>)
        [ className =? "Firefox" <&&> fmap (/="Navigator") appName
        , className =? "Nautilus" <&&> fmap (not . isSuffixOf " - File Browser") title
        , className =? "SDL_App"
        , className =? "Gimp" <&&> fmap (not . flip any ["image-window", "toolbox", "dock"] . flip isSuffixOf) role
        , fmap (=="GtkFileChooserDialog") role
        {-, fmap (/= "Google-chrome-stable") className <&&> role =? "pop-up"-}
        , fmap (isPrefixOf "sun-") appName
        , fmap (isPrefixOf "Gnuplot") title
        , flip fmap className $ flip elem
            [ "XClock"
            , "Xmessage"
            , "Floating"
            ]
        ]
    tileBelow = insertPosition Below Newer

myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> dynamicTitle myDynHook
                <+> handleEventHook def
                <+> XMonad.Layout.Fullscreen.fullscreenEventHook
  where
    myDynHook = composeAll
        [ isHangoutsFor "emacsray@" --> forceCenterFloat
        , isHangoutsFor "maskray@" --> insertPosition End Newer
        ]

myDynamicLog h = do
  copies <- wsContainingCopies
  let check ws | ws `elem` copies =
                 pad . xmobarColor yellow red . wrap "*" " "  $ ws
               | otherwise = pad ws
  ewmhDesktopsLogHook
  dynamicLogWithPP $ xmobarPP
    { ppOutput  = hPutStrLn h
    , ppCurrent = xmobarColor active "" . wrap "[" "]"
    , ppVisible = xmobarColor base0  "" . wrap "(" ")"
    , ppHidden = check
    , ppUrgent = xmobarColor red    "" . wrap " " " "
    , ppSort    = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
    , ppTitle   = xmobarColor active "" . shorten 50
    }

{-
 - Bindings
 -}

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> Flex.mouseWindow Flex.position w))
    , ((modm, button2), (\w -> focus w >> Flex.mouseWindow Flex.linear w))
    , ((modm, button3), (\w -> focus w >> Flex.mouseWindow Flex.resize w))
    ]

-- hidden, non-empty workspaces less scratchpad
nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

myNav2DConf = def { defaultTiledNavigation = centerNavigation
                  , layoutNavigation = [("Full", centerNavigation)]
                  , unmappedWindowRect = [("Full", singleWindowRect)] }

myTerminal = "termite"

myKeys =
    let
      toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                     then W.sink w s
                                     else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
      toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
         [] -> windows copyToAll
         _ -> killAllOtherCopies
      tryMsgR x y = sequence_ [(tryMessage_ x y), refresh]
      zipKey0 m ks as f = zipWith (\k d -> (m ++ k, f d)) ks as
      zipKey1 m ks as f b = zipWith (\k d -> (m ++ k, f d b)) ks as
      arrowKeys = map (wrap "<" ">" . show) dirs
      dirKeys = ["h","j","k","l"]
      dirs = [L,D,U,R]
      wsKeys = map show $ [1..9] ++ [0]
    in
    ----- Workspace / Screen
    zipKey0 "M-" wsKeys [0..] (withNthWorkspace W.greedyView) ++
    zipKey0 "M-S-" wsKeys [0..] (withNthWorkspace W.shift) ++
    zipKey0 "M-C-" wsKeys [0..] (withNthWorkspace copy) ++
    zipKey1 "M-" dirKeys dirs windowGo True ++
    zipKey1 "M-S-" dirKeys dirs windowSwap True ++
    zipKey0 "M-C-" dirKeys dirs (sendMessage . pullGroup) ++
    [ ("M-" ++ m ++ [k], f i)
        | (i, k) <- zip myWorkspaces "uiop"
        , (f, m) <- [ (windows . W.greedyView, "")
                    , (windows . liftM2 (.) W.view W.shift, "S-")
                    ]
    ]
    ++
    [ ("M-n", nextScreen)] ++
    [ ("M-S-n", swapNextScreen)] ++
    [ -- ("M-y", switchProjectPrompt warmPromptTheme)
      ("M-S-y", shiftToProjectPrompt warmPromptTheme)
    , ("M-`", nextNonEmptyWS)
    , ("M-S-`", prevNonEmptyWS)
    ]
    ++
    [("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip ["w", "e", "r"] [0..]
        , (f, m) <- [(W.view, ""), (liftM2 (.) W.view W.shift, "S-")]
    ]
    ++

    ----- Exit / Recompile

    [ ("M-S-q", io exitFailure)
    , ("M-q", spawn "ghc -e ':m +XMonad Control.Monad System.Exit' -e 'flip unless exitFailure =<< recompile False' && xmonad --restart")

    ----- Workspace / Project

    , ("M-a", toggleWS' ["NSP"])
    , ("M-s", switchProjectPrompt warmPromptTheme)
    , ("M-S-s", shiftToProjectPrompt warmPromptTheme)

    ----- Layout

    , ("M-<Tab>", sendMessage NextLayout)
    , ("M-C-<Tab>", toSubl NextLayout)
    , ("M-t", withFocused toggleFloat)
    , ("M-S-t", sinkAll)
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))

    ----- Resize

    , ("M-[", tryMsgR (ExpandTowards L) Shrink)
    , ("M-]", tryMsgR (ExpandTowards R) Expand)
    , ("M-S-[", tryMsgR (ExpandTowards U) MirrorShrink)
    , ("M-S-]", tryMsgR (ExpandTowards D) MirrorExpand)

    , ("M-S-<L>", withFocused (keysResizeWindow (-30,0) (0,0))) --shrink float at right
    , ("M-S-<R>", withFocused (keysResizeWindow (30,0) (0,0))) --expand float at right
    , ("M-S-<D>", withFocused (keysResizeWindow (0,30) (0,0))) --expand float at bottom
    , ("M-S-<U>", withFocused (keysResizeWindow (0,-30) (0,0))) --shrink float at bottom
    , ("M-C-<L>", withFocused (keysResizeWindow (30,0) (1,0))) --expand float at left
    , ("M-C-<R>", withFocused (keysResizeWindow (-30,0) (1,0))) --shrink float at left
    , ("M-C-<U>", withFocused (keysResizeWindow (0,30) (0,1))) --expand float at top
    , ("M-C-<D>", withFocused (keysResizeWindow (0,-30) (0,1))) --shrink float at top
    , ("M-<L>", withFocused (keysMoveWindow (-30,0)))
    , ("M-<R>", withFocused (keysMoveWindow (30,0)))
    , ("M-<U>", withFocused (keysMoveWindow (0,-30)))
    , ("M-<D>", withFocused (keysMoveWindow (0,30)))

    ----- Window

    , ("M-<Backspace>", kill)
    , ("M-S-<Backspace>", confirmPrompt hotPromptTheme "kill all" $ killAll)
    , ("M-'", bindOn LD [("Tabs", windows W.focusDown), ("", onGroup W.focusDown')])
    , ("M-;", bindOn LD [("Tabs", windows W.focusUp), ("", onGroup W.focusUp')])
    , ("M-\"", windows W.swapDown)
    , ("M-:", windows W.swapUp)
    , ("M-b", promote)
    , ("M-d", toggleCopyToAll)
    , ("M-S-f", placeFocused $ withGaps (22, 0, 0, 0) $ smart (0.5,0.5))
    , ("M-C-m", withFocused (sendMessage . MergeAll))
    , ("M-C-u", withFocused (sendMessage . UnMerge))
    , ("M-z m", windows W.focusMaster)
    , ("M-z u", focusUrgent)
    , ("M-S-b", banishScreen LowerRight)

    , ("M-g", windows W.focusDown)

    ----- Utility

    , ("<Print>", spawn "import -silent -quality 100 /tmp/screen.jpg")
    , ("C-<Print>", spawn "import -silent window root /tmp/screen.jpg")
    , ("M-<Return>", spawn "termite" >> sendMessage (JumpToLayout "ResizableTall"))
    , ("M-C-i", spawn "toggle-invert")
    , ("M-v", spawn $ "sleep .2 ; xdotool type --delay 0 --clearmodifiers \"$(xclip -o)\"")

    --, ("M-m h", withFocused hideWindow)
    --, ("M-m S-h", popOldestHiddenWindow)
    , ("M-m b", spawnSelected def [ "xbacklight =30"
                                  , "xbacklight =40"
                                  , "xbacklight =20"
                                  , "xbacklight =10"
                                  , "xbacklight =15"
                                  , "xbacklight =50"
                                  , "xbacklight =60"
                                  , "xbacklight =5"
                                  ])
    , ("M-m m", spawnSelected def ["zsh -c 'xdg-open /tmp/*(om[1])'", "audacity", "wireshark-gtk", "ida", "ida64", "winecfg"])
    , ("M-m k", spawn "xkill")
    , ("M-m l", spawn "xscreensaver-command -lock")

    , ("<XF86MonBrightnessUp>", spawn "change_backlight up")
    , ("<XF86MonBrightnessDown>", spawn "change_backlight down")
    , ("<XF86AudioNext>", spawn "cmus-remote -n")
    , ("<XF86AudioPrev>", spawn "cmus-remote -r")
    , ("<XF86AudioRaiseVolume>", spawn "change_volume up")
    , ("<XF86AudioLowerVolume>", spawn "change_volume down")
    , ("<XF86AudioMute>", spawn "change_volume toggle")
    , ("<XF86AudioPlay>", spawn "cmus-remote -p")
    , ("<XF86AudioPause>", spawn "cmus-remote -u")
    , ("<XF86Display>", spawn "xset dpms force standby")
    , ("<XF86Eject>", spawn "eject")

    --, ("M-S-<L>", withFocused (keysResizeWindow (-30,0) (0,0))) --shrink float at right
    --, ("M-S-<R>", withFocused (keysResizeWindow (30,0) (0,0))) --expand float at right
    --, ("M-S-<D>", withFocused (keysResizeWindow (0,30) (0,0))) --expand float at bottom
    --, ("M-S-<U>", withFocused (keysResizeWindow (0,-30) (0,0))) --shrink float at bottom
    --, ("M-C-<L>", withFocused (keysResizeWindow (30,0) (1,0))) --expand float at left
    --, ("M-C-<R>", withFocused (keysResizeWindow (-30,0) (1,0))) --shrink float at left
    --, ("M-C-<U>", withFocused (keysResizeWindow (0,30) (0,1))) --expand float at top
    --, ("M-C-<D>", withFocused (keysResizeWindow (0,-30) (0,1))) --shrink float at top
    -- , ("M-<L>", withFocused (keysMoveWindow (-30,0)))
    -- , ("M-<R>", withFocused (keysMoveWindow (30,0)))
    -- , ("M-<U>", withFocused (keysMoveWindow (0,-30)))
    -- , ("M-<D>", withFocused (keysMoveWindow (0,30)))
    --, ("C-; <L>", withFocused $ snapMove L Nothing)
    --, ("C-; <R>", withFocused $ snapMove R Nothing)
    --, ("C-; <U>", withFocused $ snapMove U Nothing)
    --, ("C-; <D>", withFocused $ snapMove D Nothing)

    -- Volume
    --, ("C-; 9", spawn "change_volume down")
    --, ("C-; 0", spawn "change_volume up")
    --, ("C-; m", spawn "change_volume toggle")

    -- preferred cui programs
    -- , ("C-; C-;", pasteChar controlMask ';')
    , ("C-' C-'", pasteChar controlMask '\'')
    , ("C-' a", namedScratchpadAction scratchpads "alsamixer")
    , ("C-' c", namedScratchpadAction scratchpads "cmus")
    , ("C-' d", namedScratchpadAction scratchpads "goldendict")
    , ("C-' e", namedScratchpadAction scratchpads "erl")
    , ("C-' g", namedScratchpadAction scratchpads "gp")
    , ("C-' h", namedScratchpadAction scratchpads "ghci")
    , ("C-' i", namedScratchpadAction scratchpads "idris")
    , ("C-' j", namedScratchpadAction scratchpads "j8")
    , ("C-' m", namedScratchpadAction scratchpads "sage")
    , ("C-' n", namedScratchpadAction scratchpads "node")
    , ("C-' o", namedScratchpadAction scratchpads "utop")
    , ("C-' p", namedScratchpadAction scratchpads "ipython")
    , ("C-' r", namedScratchpadAction scratchpads "pry")
    , ("C-' s", namedScratchpadAction scratchpads "ydcv")
    , ("C-' t", namedScratchpadAction scratchpads "htop")
    , ("C-' u", namedScratchpadAction scratchpads "R")
    , ("C-' w", namedScratchpadAction scratchpads "writefull")
    , ("C-' z", namedScratchpadAction scratchpads "zeal")

    , ("M-C-<Space>", sendMessage $ Toggle NBFULL)
    , ("M-C-t", sendMessage $ Toggle TABBED)
    , ("M-C-x", sendMessage $ Toggle REFLECTX)
    , ("M-C-y", sendMessage $ Toggle REFLECTY)
    , ("M-C-z", sendMessage $ Toggle MIRROR)
    , ("M-C-b", sendMessage $ Toggle NOBORDERS)

    -- prompts
    , ("M-y b", windowPromptBring myXPConfig)
    , ("M-y c", mainCommandPrompt myXPConfig)
    , ("M-y d", spawn "rofi -sort -matching fuzzy -show file -modi file:\"rofi-file-browser $HOME/Documents\"")
    , ("M-y p", spawn "rofi -sort -matching fuzzy -show file -modi file:\"rofi-file-browser $HOME/Papers\"")
    , ("M-y r", spawn "rofi -sort -matching fuzzy -show run")
    , ("M-<Space>", spawn "rofi -sort -matching fuzzy -show run")
    , ("M-y t", spawn "rofi -sort -matching fuzzy -show file -modi file:\"rofi-file-browser /tmp\"")
    , ("M-y v", spawn "pavucontrol")
    , ("M-y m", spawn "menu")
    ] ++
    searchBindings

mlterm prog = ("mlterm -T "++) . ((++) . head $ words prog) . (" -e "++) . (prog++) $ ""
termite prog = ("termite -t "++) . ((++) . head $ words prog) . (" -e '"++) . (prog++) $ "'"

scratchpads =
  map f ["alsamixer", "cmus", "erl", "gp", "htop", "idris", "ipython", "j8 -c", "node --harmony", "pry", "R", "sage", "utop", "xosview", "ydcv"] ++
  [ NS "ghci" "termite -t ghci -e 'zsh -c \"stack ghci || ghci\"'" (title =? "ghci") doSPFloat
  , NS "goldendict" "goldendict" (className =? "GoldenDict") doSPFloat
  , NS "writefull" "~/.local/opt/writefull/Writefull" (title =? "Writefull") doSPFloat
  ]
  where
    f cmd = NS name (termite cmd) (fmap (name ==) title) doSPFloat
      where
        name = head $ words cmd
    doTopFloat = customFloating $ W.RationalRect (1/3) 0 (1/3) (1/3)
    doTopLeftFloat = customFloating $ W.RationalRect 0 0 (1/3) (1/3)
    doTopRightFloat = customFloating $ W.RationalRect (2/3) 0 (1/3) (1/3)
    doBottomLeftFloat = customFloating $ W.RationalRect 0 (2/3) (1/3) (1/3)
    doBottomRightFloat = customFloating $ W.RationalRect (2/3) (2/3) (1/3) (1/3)
    doLeftFloat = customFloating $ W.RationalRect 0 0 (1/3) 1
    orgFloat = customFloating $ W.RationalRect (1/2) (1/2) (1/2) (1/2)

myConfig xmobar = docks . dynamicProjects myProjects . withNavigation2DConfig myNav2DConf . ewmh $ withUrgencyHook LibNotifyUrgencyHook $ def
    { terminal           = myTerminal
    , focusFollowsMouse  = False
    , borderWidth        = 1
    , modMask            = mod4Mask
    , workspaces         = myWorkspaces
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#3939ff"
    , mouseBindings      = myMouseBindings
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = myHandleEventHook -- <+> focusFollow -- >> clockEventHook
    , logHook            = historyHook >> myDynamicLog xmobar
    , startupHook        = checkKeymap (myConfig xmobar) myKeys >> spawn "~/bin/start-tiling" >> setWMName "LG3D"
} `additionalKeysP` myKeys

myPromptKeymap = M.union defaultXPKeymap $ M.fromList
                 [
                   ((controlMask, xK_g), quit)
                 , ((controlMask, xK_m), setSuccess True >> setDone True)
                 , ((controlMask, xK_j), setSuccess True >> setDone True)
                 , ((controlMask, xK_h), deleteString Prev)
                 , ((controlMask, xK_f), moveCursor Next)
                 , ((controlMask, xK_b), moveCursor Prev)
                 , ((controlMask, xK_p), moveHistory W.focusDown')
                 , ((controlMask, xK_n), moveHistory W.focusUp')
                 , ((mod1Mask, xK_p), moveHistory W.focusDown')
                 , ((mod1Mask, xK_n), moveHistory W.focusUp')
                 , ((mod1Mask, xK_b), moveWord Prev)
                 , ((mod1Mask, xK_f), moveWord Next)
                 ]

myXPConfig = def
    { font = "xft:DejaVu Sans Mono:pixelsize=16"
    , bgColor           = "#0c1021"
    , fgColor           = "#f8f8f8"
    , fgHLight          = "#f8f8f8"
    , bgHLight          = "steelblue3"
    , borderColor       = "DarkOrange"
    , promptBorderWidth = 1
    , position          = Top
    , historyFilter     = deleteConsecutive
    , promptKeymap = myPromptKeymap
    }

-- | Like 'spawn', but uses bash and returns the 'ProcessID' of the launched application
spawnBash :: MonadIO m => String -> m ProcessID
spawnBash x = xfork $ executeFile "/bin/bash" False ["-c", encodeString x] Nothing

main = do
    d <- openDisplay ""
    let w = fromIntegral $ displayWidth d 0 :: Int
        h = fromIntegral $ displayHeight d 0 :: Int
    let barWidth = 160 --h `div` 12
    let barHeight = h `div` 35
    let fontSize = h `div` 54
    xmobar <- spawnPipe "killall xmobar; xmobar"
    spawn $ "killall trayer; trayer --align left --edge top --expand false --width " ++ show barWidth ++ " --transparent true --tint 0x000000 --widthtype pixel --SetPartialStrut true --SetDockType true --height 32"
    xmonad $ myConfig xmobar

{-
 - SearchMap
 -}

searchBindings = [ ("M-S-/", S.promptSearch myXPConfig multi) ] ++
                 [ ("M-/ " ++ name, S.promptSearch myXPConfig e) | e@(S.SearchEngine name _) <- engines, length name == 1 ]
  where
    promptSearch (S.SearchEngine _ site)
      = inputPrompt myXPConfig "Search" ?+ \s ->
      (S.search "chromium" site s >> viewWeb)
    viewWeb = windows (W.view "web")

    mk = S.searchEngine
    engines = [ mk "h" "http://www.haskell.org/hoogle/?q="
      , mk "g" "http://www.google.com/search?num=100&q="
      , mk "t" "http://developer.gnome.org/search?q="
      , mk "w" "http://en.wikipedia.org/wiki/Special:Search?go=Go&search="
      , mk "d" "http://duckduckgo.com/?q="
      , mk "m" "https://developer.mozilla.org/en-US/search?q="
      , mk "r" "http://www.ruby-doc.org/search.html?sa=Search&q="
      , mk "p" "http://docs.python.org/search.html?check_keywords=yes&area=default&q="
      , mk "s" "https://scholar.google.de/scholar?q="
      , mk "i" "https://ixquick.com/do/search?q="
      , mk "dict" "http://www.dict.cc/?s="
      , mk "imdb" "http://www.imdb.com/find?s=all&q="
      , mk "def" "http://www.google.com/search?q=define:"
      , mk "img" "http://images.google.com/images?q="
      , mk "gh" "https://github.com/search?q="
      , mk "bb" "https://bitbucket.org/repo/all?name="
      , mk "alpha" "http://www.wolframalpha.com/input/i="
      , mk "ud" "http://www.urbandictionary.com/define.php?term="
      , mk "rtd" "http://readthedocs.org/search/project/?q="
      , mk "null" "http://nullege.com/codes/search/"
      , mk "sf" "http://sourceforge.net/search/?q="
      , mk "acm" "https://dl.acm.org/results.cfm?query="
      , mk "math" "http://mathworld.wolfram.com/search/?query="
      ]
    multi = S.namedEngine "multi" $ foldr1 (S.!>) engines

wsWeb = "web"
wsGen = "gen"
wsIM = "im"
wsEmacs = "emacs"

wsFloat = "float"
wsGimp = "gimp"
wsIda = "ida"
wsIda64 = "i64"
wsInkscape = "inkscape"
wsMail = "mail"

myProjects :: [Project]
myProjects =
  [ Project wsWeb "~" . Just $ spawn "chromium"
  , Project wsGen "~" . Just $ spawn (termite "tmux new -As default")
  , Project wsIM "~" . Just $ spawn "termite -e 'env SSH_AUTH_SOCK= ssh -R 9010:0:9010 -tX linode-ca \"tmux new -As weechat\"'"
  , Project wsEmacs "~" . Just $ spawn "LC_CTYPE=zh_CN.UTF-8 emacs"

  , Project wsIda "/tmp" . Just $ spawn "ida" >> spawn (termite "tmux new -As ida")
  , Project wsIda64 "/tmp" . Just $ spawn "ida64" >> spawn (termite "tmux new -As ida")
  , Project wsMail "/tmp" . Just $ spawn (termite "mutt")
  , Project wsGimp "/tmp" . Just $ spawn "gimp"
  , Project wsInkscape "/tmp" . Just $ spawn "inkscape"
  ]

myWorkspaces = map projectName myProjects

myCommands =
    [ ("wallpaper", safeSpawn "change-wallpaper" [])
    --, ("fade", fadePrompt myXPConfig)
    ]

data TitledPrompt = TitledPrompt String

instance XPrompt TitledPrompt where
    showXPrompt (TitledPrompt t)  = t ++ ": "
    commandToComplete _ c   = c
    nextCompletion    _     = getNextCompletion

mkCommandPrompt :: XPConfig -> [(String, X ())] -> X ()
mkCommandPrompt xpc cs = do
    mkXPrompt (TitledPrompt "Command") xpc compl $ \i -> whenJust (find ((==i) . fst) cs) snd
  where
    compl s = return . filter (searchPredicate xpc s) . map fst $ cs

mainCommandPrompt xpc = do
  defs <- defaultCommands
  mkCommandPrompt xpc $ nubBy ((==) `on` fst) $ myCommands ++ defs

getFilesWithExt :: [String] -> String -> IO [String]
getFilesWithExt exts s = fmap lines $ runProcessWithInput "sh" [] ("ls -d -- " ++ s ++ "*/ " ++ s ++ "*." ++ f ++ "\n")
  where
    f = if length exts == 1 then head exts else ('{':) . (++"}") $ intercalate "," exts

{- | Get the user's response to a prompt an launch an application using the
   input as command parameters of the application.-}
launchApp :: XPConfig -> String -> [String] -> X ()
launchApp config app exts = mkXPrompt (TitledPrompt app) config (getFilesWithExt exts) $ launch app
  where
    launch :: MonadIO m => String -> String -> m ()
    launch app params = spawn $ app ++ " " ++ completionToCommand (undefined :: Shell) params

-- from:
-- https://github.com/pjones/xmonadrc/blob/master/src/XMonad/Local/Action.hs
--
-- Useful when a floating window requests stupid dimensions.  There
-- was a bug in Handbrake that would pop up the file dialog with
-- almost no height due to one of my rotated monitors.

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 3/5
    h = 3/5
    x = (1-w)/2
    y = (1-h)/2

--- XMonad.Actions.ConditionalKeys

data XCond = WS | LD

-- | Choose an action based on the current workspace id (WS) or
-- layout description (LD).
chooseAction :: XCond -> (String->X()) -> X()
chooseAction WS f = withWindowSet (f . W.currentTag)
chooseAction LD f = withWindowSet (f . description . W.layout . W.workspace . W.current)


-- | If current workspace or layout string is listed, run the associated
-- action (only the first match counts!) If it isn't listed, then run the default
-- action (marked with empty string, \"\"), or do nothing if default isn't supplied.
bindOn :: XCond -> [(String, X())] -> X()
bindOn xc bindings = chooseAction xc $ chooser where
    chooser xc = case find ((xc==).fst) bindings of
        Just (_, action) -> action
        Nothing -> case find ((""==).fst) bindings of
            Just (_, action) -> action
            Nothing -> return ()

