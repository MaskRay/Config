local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local projects = require 'projects'

config.hide_tab_bar_if_only_one_tab = true
config.harfbuzz_features = {"calt=0", "clig=0", "liga=0"}

config.color_scheme = 'Catppuccin Mocha'
config.font = wezterm.font 'FantasqueSansM Nerd Font'
config.font_size = 18

config.leader = { mods = 'SUPER', key = 'm', timeout_milliseconds = 1000 }
config.keys = {
  {mods = 'LEADER|CTRL', key = 'a', action = wezterm.action.SendKey { key = 'a', mods = 'CTRL'}},
  {mods = 'LEADER', key = 'v', action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain'}},
  {mods = 'LEADER', key = "'", action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain'}},
  {mods = 'LEADER', key = 'h', action = wezterm.action {ActivatePaneDirection='Left'}},
  {mods = 'LEADER', key = 'j', action = wezterm.action {ActivatePaneDirection='Down'}},
  {mods = 'LEADER', key = 'k', action = wezterm.action {ActivatePaneDirection='Up'}},
  {mods = 'LEADER', key = 'l', action = wezterm.action {ActivatePaneDirection='Right'}},

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
