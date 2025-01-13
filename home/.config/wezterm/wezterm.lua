local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local projects = require 'projects'
local act = wezterm.action

config.hide_tab_bar_if_only_one_tab = true
config.harfbuzz_features = {"calt=0", "clig=0", "liga=0"}

-- macOS: disable AltGr
config.send_composed_key_when_right_alt_is_pressed = false

config.color_scheme = 'Catppuccin Mocha'
config.font = wezterm.font 'FantasqueSansM Nerd Font'
config.font_size = 18

config.leader = { mods = 'SUPER', key = 'm', timeout_milliseconds = 1000 }
config.keys = {
  {mods = 'LEADER|CTRL', key = 'a', action = act.SendKey { key = 'a', mods = 'CTRL'}},
  {mods = 'LEADER', key = 'v', action = act.SplitHorizontal { domain = 'CurrentPaneDomain'}},
  {mods = 'LEADER', key = "'", action = act.SplitVertical { domain = 'CurrentPaneDomain'}},
  {mods = 'LEADER', key = 'h', action = act {ActivatePaneDirection='Left'}},
  {mods = 'LEADER', key = 'j', action = act {ActivatePaneDirection='Down'}},
  {mods = 'LEADER', key = 'k', action = act {ActivatePaneDirection='Up'}},
  {mods = 'LEADER', key = 'l', action = act {ActivatePaneDirection='Right'}},
  {mods = 'CTRL|SHIFT', key = 't', action = act.SpawnTab 'CurrentPaneDomain'},
  {mods = 'CTRL|SHIFT', key = 'w', action = act.CloseCurrentTab {confirm=true}},

   -- copy, paste, search --
   { key = 'C', mods = 'SUPER', action = act.CopyTo 'Clipboard' },
   { key = 'V', mods = 'SUPER', action = act.PasteFrom 'Clipboard' },
   { key = 'F', mods = 'SHIFT|CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },
   { key = ' ', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },

  {
    key = 'p',
    mods = 'LEADER',
    -- Present in to our project picker
    action = projects.choose_project(),
  },
    {
    key = 'f',
    mods = 'LEADER',
    -- Present a list of existing workspaces
    action = wezterm.action.ShowLauncherArgs { flags = 'FUZZY|WORKSPACES' },
  },
}

return config
