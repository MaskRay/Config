import Control.Monad
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe (isNothing, isJust, catMaybes)
import Data.Monoid
import System.Exit
import System.IO
import Text.Regex

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Util.NamedWindows (getName)

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.TopicSpace
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.WindowMenu
import XMonad.Actions.WithAll (killAll)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.Master
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WorkspaceDir
import qualified XMonad.Layout.Magnifier as Mag

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste


myLayout = avoidStruts $
    gaps (zip [U,D,L,R] (repeat 0)) $
    workspaceDir "~" $
    configurableNavigation (navigateColor "#00aa00") $
    mkToggle1 NBFULL $
    mkToggle1 REFLECTX $
    mkToggle1 REFLECTY $
    mkToggle1 MIRROR $
    mkToggle1 NOBORDERS $
    smartBorders $
    onWorkspaces ["web","irc"] (Full ||| myTiled) $
    myTiled ||| Mag.magnifier Grid ||| TwoPane (3/100) (1/2)
    where
        myTiled = named "Tall" $ ResizableTall 1 0.03 0.5 []

myManageHook = composeAll $
    [ className =? c --> doCenterFloat | c <- myFloats]
    ++
    [ title =? t --> doFloat | t <- myTFloats]
    ++
    [ manageDocks
    , namedScratchpadManageHook scratchpads
    ]
    where
        myFloats = [ "feh"
                   , "Display"
                   , "XClock"
                   , "Xmessage"
                   , "Floating"
                   ]
        myTFloats = [ "Downloads", "Add-ons", "Firefox Preferences"]

myLogHook h = dynamicLogWithPP $ defaultPP {
    ppHidden = xmobarColor "#00FF00" ""
    , ppCurrent = xmobarColor "#FF0000" "" . wrap "[" "]"
    , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
    , ppLayout = xmobarColor "#FF0000" "" .
        flip (subRegex (mkRegex "ReflectX")) "[|]" .
        flip (subRegex (mkRegex "ReflectY")) "[-]" .
        flip (subRegex (mkRegex "Mirror")) "[+]"
    , ppTitle = xmobarColor "#00FF00" "" . shorten 80
    , ppOutput = hPutStrLn h
    , ppSep = "<fc=#0033FF> | </fc>"
    , ppSort = fmap (namedScratchpadFilterOutWorkspace.) (ppSort xmobarPP) -- hide "NSP" from the workspace list
    }

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myKeys =
    [ ("M-" ++ m ++ [k], f i)
        | (i, k) <- zip myTopicNames "1234567890-="
        , (f, m) <- [ (switchTopic myTopicConfig, "")
                    , (windows . W.shift, "S-")
                    ]
    ]
    ++
    [ ("C-; " ++ m ++ [k], f i)
        | (i, k) <- zip myTopicNames "asdfghjkl;'\""
        , (f, m) <- [ (switchTopic myTopicConfig, "")
                    , (windows . W.shift, "S-")
                    ]
    ]
    ++
    [("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip ["w", "e", "r"] [0..]
        , (f, m) <- [(W.view, ""), (W.shift, "S-")]
    ]
    ++
    [ ("M-S-q", io exitFailure)
    , ("M-S-c", kill)
    , ("M-q", spawn "xmonad --recompile; xmonad --restart")

    , ("<Print>", spawn "import /tmp/screen.jpg")
    , ("C-<Print>", spawn "import -window root /screen.jpg")
    , ("M-s", spawnSelected defaultGSConfig ["xterm", "firefox-bin", "emacs --daemon", "desmume", "VisualBoyAdvance "])
    , ("M-S-i", spawn "xcalib -i -a")
    , ("M-S-l", spawn "xscreensaver-command -lock")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
    , ("<XF86AudioMute>", spawn "amixer set Master mute")
    , ("<XF86AudioUnmute>", spawn "amixer set Master unmute")

    -- window management
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-i", sendMessage Shrink)
    , ("M-o", sendMessage Expand)
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))
    , ("M-b", sendMessage ToggleStruts)
    , ("M-d", bringMenu)
    , ("M-y", focusUrgent)
    , ("M-<L>", withFocused (keysMoveWindow (-20,0))) -- move float left
    , ("M-<R>", withFocused (keysMoveWindow (20,0))) -- move float right
    , ("M-<U>", withFocused (keysMoveWindow (0,-20))) -- move float up
    , ("M-<D>", withFocused (keysMoveWindow (0,20))) -- move float down
    , ("M-S-<L>", withFocused (keysResizeWindow (-20,0) (0,0))) --shrink float at right
    , ("M-S-<R>", withFocused (keysResizeWindow (20,0) (0,0))) --expand float at right
    , ("M-S-<D>", withFocused (keysResizeWindow (0,20) (0,0))) --expand float at bottom
    , ("M-S-<U>", withFocused (keysResizeWindow (0,-20) (0,0))) --shrink float at bottom
    , ("M-C-<L>", withFocused (keysResizeWindow (20,0) (1,0))) --expand float at left
    , ("M-C-<R>", withFocused (keysResizeWindow (-20,0) (1,0))) --shrink float at left
    , ("M-C-<U>", withFocused (keysResizeWindow (0,20) (0,1))) --expand float at top
    , ("M-C-<D>", withFocused (keysResizeWindow (0,-20) (0,1))) --shrink float at top

    -- workspace management
    , ("C-; ;", toggleWS)

    -- dynamic workspace
    , ("M-n", addWorkspacePrompt myXPConfig)
    , ("M-C-r", removeWorkspace)
    , ("M-C-S-r", killAll >> removeWorkspace)

    -- preferred cui programs
    , ("C-; C-;", pasteChar controlMask ';')
    , ("C-' C-'", pasteChar controlMask '\'')
    , ("C-' s", namedScratchpadAction scratchpads "screen")
    , ("C-' g", namedScratchpadAction scratchpads "ghci")
    , ("C-' h", namedScratchpadAction scratchpads "htop")
    , ("C-' o", namedScratchpadAction scratchpads "offlineimap")
    , ("C-' r", namedScratchpadAction scratchpads "r2e")
    , ("C-' a", namedScratchpadAction scratchpads "alsamixer")

    , ("M-C-<Space>", sendMessage $ Toggle NBFULL)
    , ("M-C-x",       sendMessage $ Toggle REFLECTX)
    , ("M-C-y",       sendMessage $ Toggle REFLECTY)
    , ("M-C-m",       sendMessage $ Toggle MIRROR)
    , ("M-C-b",       sendMessage $ Toggle NOBORDERS)

    -- prompts
    , ("M-'", workspacePrompt myXPConfig (switchTopic myTopicConfig) )
    , ("M-p c", prompt ("xterm -e") myXPConfig)
    , ("M-p m", manPrompt myXPConfig)
    , ("M-p d", changeDir myXPConfig)
    , ("M-p p", runOrRaisePrompt myXPConfig)
    , ("M-p M-p", runOrRaisePrompt myXPConfig)
    , ("M-/",   submap . mySearchMap $ myPromptSearch)
    , ("M-C-/", submap . mySearchMap $ mySelectSearch)


    ]

scratchpads =
  [ NS "screen" "xterm -T screen -e 'screen -d -R'" (title =? "screen") mySPFloat
  , NS "ghci" "xterm -T ghci -e ghci" (title =? "ghci") mySPFloat
  , NS "htop" "xterm -T htop -e htop" (title =? "htop") mySPFloat
  , NS "offlineimap" "xterm -T offlineimap -e 'offlineimap -o -d thread'" (title =? "offlineimap") doTopRightFloat
  , NS "r2e" "xterm -T r2e -e 'r2e run'" (title =? "r2e") doBottomRightFloat
  , NS "alsamixer" "xterm -T alsamixer -e alsamixer" (title =? "alsamixer") doLeftFloat
  ]
  where
    mySPFloat = customFloating $ W.RationalRect (1/5) (1/5) (3/5) (3/5)
    doTopLeftFloat = customFloating $ W.RationalRect 0 0 (1/3) (1/3)
    doTopRightFloat = customFloating $ W.RationalRect (2/3) 0 (1/3) (1/3)
    doBottomLeftFloat = customFloating $ W.RationalRect 0 (2/3) (1/3) (1/3)
    doBottomRightFloat = customFloating $ W.RationalRect (2/3) (2/3) (1/3) (1/3)
    doLeftFloat = customFloating $ W.RationalRect 0 0 (1/3) 1

main = do
     xmobar <- spawnPipe "/usr/bin/xmobar"
     checkTopicConfig myTopicNames myTopicConfig
     xmonad $ ewmh $ withUrgencyHook LibNotifyUrgencyHook $ defaultConfig {
        terminal           = "xterm",
        focusFollowsMouse  = True,
        borderWidth        = 1,
        modMask            = mod4Mask,
        workspaces         = myTopicNames,
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "#ff0000",
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = mempty,
        logHook            = myLogHook xmobar <+> fadeInactiveLogHook 0.4 <+> updatePointer (Relative 0.5 0.5),
        startupHook        = spawn "~/bin/start-tiling"
    } `additionalKeysP` myKeys

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                safeSpawn "notify-send" [show name ++ " requests your attention on workspace " ++ index]

myXPConfig = defaultXPConfig
    { font = "xft:DejaVu Sans Mono:pixelsize=16"
    , bgColor           = "#0c1021"
    , fgColor           = "#f8f8f8"
    , fgHLight          = "#f8f8f8"
    , bgHLight          = "steelblue3"
    , borderColor       = "DarkOrange"
    , promptBorderWidth = 1
    , position          = Top
    }



-- Perform a search, using the given method, based on a keypress
mySearchMap method = M.fromList $
        [ ((0, xK_g), method google)
        , ((0, xK_w), method wikipedia)
        , ((0, xK_h), method hoogle)
        , ((shiftMask, xK_h), method hackage)
        , ((0, xK_s), method scholar)
        , ((0, xK_m), method mathworld)
        , ((0, xK_p), method maps)
        , ((0, xK_d), method dictionary)
        , ((0, xK_a), method alpha)
        , ((0, xK_l), method lucky)
        , ((0, xK_i), method images)
        ]

myPromptSearch (SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ \s ->
      (search "firefox" site s >> viewWeb)

mySelectSearch eng = selectSearch eng >> viewWeb

viewWeb = windows (W.view "web")



data TopicItem = TI { topicName :: Topic
                    , topicDir  :: Dir
                    , topicAction :: X ()
                    }

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $ map (\(TI n d _) -> (n,d)) myTopics
    , defaultTopicAction = const (return ())
    , defaultTopic = "web"
    , maxTopicHistory = 10
    , topicActions = M.fromList $ map (\(TI n _ a) -> (n,a)) myTopics
    }

myTopics :: [TopicItem]
myTopics =
    [ TI "web" "" (spawn "firefox")
    , TI "code" "" (spawn "gvim")
    , TI "mail" "" (spawn "xterm -T mutt -e mutt")
    , TI "doc" "Documents/" (spawn "evince")
    , TI "net" "" (spawn "wpa_gui")
    , TI "dict" "" (spawn "goldendict")
    , TI "irc" "" (spawn "xterm -T irssi -e irssi")
    , TI "org" "org/" (spawn "gvim ~/org/`date +%Y-%m-%d`.txt")
    ]
