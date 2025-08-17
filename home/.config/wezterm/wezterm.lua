local wezterm = require 'wezterm'
local config = wezterm.config_builder()
-- local projects = require 'projects'
local act = wezterm.action
local is_mac = wezterm.target_triple:find("darwin") ~= nil

local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")

config.hide_tab_bar_if_only_one_tab = true
config.harfbuzz_features = {"calt=0", "clig=0", "liga=0"}
config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }

config.unix_domains = { { name = 'unix', }, }

-- macOS: disable AltGr
config.send_composed_key_when_right_alt_is_pressed = false

config.color_scheme = 'Catppuccin Mocha'
config.font = wezterm.font 'FantasqueSansM Nerd Font'
config.font_size = is_mac and 24 or 18

config.leader = {mods = 'ALT', key = 'Space'}
config.disable_default_key_bindings = is_mac
config.keys = {
  { key = 'Space', mods = "LEADER|CTRL", action = act.SendKey { key = "Space", mods = "CTRL" } },
  -- Disable QuickSelect
  { key = 'Space', mods = 'CTRL|SHIFT', action = act.DisableDefaultAssignment},

  --= Tabs
  { key = 'c', mods = 'LEADER', action = act.SpawnTab 'CurrentPaneDomain'},
  { key = 'p', mods = 'LEADER', action = act.ActivateTabRelative(-1)},
  { key = 'n', mods = 'LEADER', action = act.ActivateTabRelative(1)},
  { key = 'W', mods = 'CTRL', action = act.CloseCurrentTab {confirm=true}},
  { key = '{', mods = 'SHIFT|CTRL', action = act.MoveTabRelative(-1) },
  { key = '}', mods = 'SHIFT|CTRL', action = act.MoveTabRelative(1) },

  --= Panes
  { key = 'v', mods = 'LEADER', action = act.SplitHorizontal { domain = 'CurrentPaneDomain'}},
  { key = "'", mods = 'LEADER', action = act.SplitVertical { domain = 'CurrentPaneDomain'}},
  { key = 'z', mods = 'LEADER', action = act.TogglePaneZoomState },

  -- Navigate panes
  {
    key = 'h', mods = 'LEADER',
    action = wezterm.action_callback(function(window, pane)
      local tab = window:mux_window():active_tab()
      if tab:get_pane_direction('Left') ~= nil then
        window:perform_action(wezterm.action.ActivatePaneDirection('Left'), pane)
      else
        window:perform_action(wezterm.action.ActivateTabRelative(-1), pane)
      end
    end),
  },
  {
    key = 'l', mods = 'LEADER',
    action = wezterm.action_callback(function(window, pane)
      local tab = window:mux_window():active_tab()
      if tab:get_pane_direction('Right') ~= nil then
        window:perform_action(wezterm.action.ActivatePaneDirection('Right'), pane)
      else
        window:perform_action(wezterm.action.ActivateTabRelative(1), pane)
      end
    end),
  },
  { key = 'j', mods = 'LEADER', action = act {ActivatePaneDirection='Down'}},
  { key = 'k', mods = 'LEADER', action = act {ActivatePaneDirection='Up'}},

  -- Resize pane
  { key = 'H', mods = 'LEADER', action = act.AdjustPaneSize {'Left', 3}},
  { key = 'J', mods = 'LEADER', action = act.AdjustPaneSize {'Down', 3}},
  { key = 'K', mods = 'LEADER', action = act.AdjustPaneSize {'Up', 3}},
  { key = 'L', mods = 'LEADER', action = act.AdjustPaneSize {'Right', 3}},

  -- copy, paste, search --
  { key = 'x', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },
  { key = 'c', mods = 'SUPER', action = act.CopyTo 'Clipboard' },
  { key = 'v', mods = 'SUPER', action = act.PasteFrom 'PrimarySelection' },
  { key = 'V', mods = 'CTRL', action = act.PasteFrom 'PrimarySelection' },
  { key = 'f', mods = 'SHIFT|CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },

  --= Session
  { key = 'a', mods = 'LEADER', action = act.AttachDomain 'unix', },
  { key = 'd', mods = 'LEADER', action = act.DetachDomain { DomainName = 'unix' } },

  -- {
  --   key = 'p',
  --   mods = 'LEADER',
  --   -- Present in to our project picker
  --   action = projects.choose_project(),
  -- },
  { key = "s", mods = "CTRL|SHIFT", action = workspace_switcher.switch_workspace() },
  { key = 't', mods = 'CTRL|SHIFT', action = wezterm.action.ShowLauncherArgs { flags = 'FUZZY|WORKSPACES' }},
  { key = "[", mods = "LEADER", action = act.SwitchWorkspaceRelative(-1) },
  { key = "]", mods = "LEADER", action = act.SwitchWorkspaceRelative(1) },
}

for i = 1,9 do
  config.keys[#config.keys+1] = {mods = 'CTRL|SHIFT', key = tostring(i), action = act.ActivateTab(i-1) }
  config.keys[#config.keys+1] = {mods = 'LEADER', key = tostring(i), action = act.ActivateTab(i-1) }
end

return config
