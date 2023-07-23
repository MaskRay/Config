local wezterm = require 'wezterm'
local config = {}

config.hide_tab_bar_if_only_one_tab = true
config.harfbuzz_features = {"calt=0", "clig=0", "liga=0"}

return config
