# -*- coding: utf-8 -*-
#
# Copyright (C) 2009-2012 Sebastien Helleu <flashcode@flashtux.org>
# Copyright (C) 2010 m4v <lambdae2@gmail.com>
# Copyright (C) 2011 stfn <stfnmd@googlemail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#
# Quick jump to buffers.
# (this script requires WeeChat 0.3.0 or newer)
#
# History:
# 2012-01-03 nils_2 <weechatter@arcor.de>
#     version 1.7: add option use_core_instead_weechat (requested by k-man and _rane)
# 2012-01-03, Sebastien Helleu <flashcode@flashtux.org>:
#     version 1.6: make script compatible with Python 3.x
# 2011-08-24, stfn <stfnmd@googlemail.com>:
#     version 1.5: /go with name argument jumps directly to buffer
#                  Remember cursor position in buffer input
# 2011-05-31, Elián Hanisch <lambdae2@gmail.com>:
#     version 1.4: Sort list of buffers by activity.
# 2011-04-25, Sebastien Helleu <flashcode@flashtux.org>:
#     version 1.3: add info "go_running" (used by script input_lock.rb)
# 2010-11-01, Sebastien Helleu <flashcode@flashtux.org>:
#     version 1.2: use high priority for hooks to prevent conflict with other
#                  plugins/scripts (WeeChat >= 0.3.4 only)
# 2010-03-25, Elián Hanisch <lambdae2@gmail.com>:
#     version 1.1: use a space for match the end of a string
# 2009-11-16, Sebastien Helleu <flashcode@flashtux.org>:
#     version 1.0: add new option for displaying short names
# 2009-06-15, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.9: fix typo in /help go with command /key
# 2009-05-16, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.8: search buffer by number, fix bug when window is split
# 2009-05-03, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.7: eat tab key (do not complete input, just move buffer pointer)
# 2009-05-02, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.6: sync with last API changes
# 2009-03-22, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.5: update modifier signal name for input text display,
#                  fix arguments for function string_remove_color
# 2009-02-18, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.4: do not hook command and init options if register failed
# 2009-02-08, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.3: case insensitive search for buffers names
# 2009-02-08, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.2: add help about Tab key
# 2009-02-08, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.1: initial release
#

import weechat, re

SCRIPT_NAME    = "go"
SCRIPT_AUTHOR  = "Sebastien Helleu <flashcode@flashtux.org>"
SCRIPT_VERSION = "1.7"
SCRIPT_LICENSE = "GPL3"
SCRIPT_DESC    = "Quick jump to buffers"

# script options
settings = {
    "color_number"                 : "yellow,magenta",
    "color_number_selected"        : "yellow,red",
    "color_name"                   : "black,cyan",
    "color_name_selected"          : "black,brown",
    "color_name_highlight"         : "red,cyan",
    "color_name_highlight_selected": "red,brown",
    "message"                      : "Go to: ",
    "short_name"                   : "off",
    "sort_by_activity"             : "off",
    "use_core_instead_weechat"     : "off",
}

# hooks management
hook_command_run = {
    "input" : ("/input *",  "command_run_input"),
    "buffer": ("/buffer *", "command_run_buffer"),
    "window": ("/window *", "command_run_window"),
}
hooks = {}

# input before command /go (we'll restore it later)
saved_input = ""
saved_input_pos = 0

# last user input (if changed, we'll update list of matching buffers)
old_input = None

# matching buffers
buffers = []
buffers_pos = 0

if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
                    SCRIPT_DESC, "go_unload_script", ""):
    weechat.hook_command("go", "Quick jump to buffers", "[name]",
                         "name: directly jump to buffer by name (without argument, list is displayed)\n\n" +
                         "You can bind command to a key, for example:\n" +
                         "  /key bind meta-g /go\n\n" +
                         "You can use completion key (commonly Tab and shift-Tab) to select " +
                         "next/previous buffer in list.",
                         "%(buffers_names)", "go_cmd", "")
    for option, default_value in settings.items():
        if weechat.config_get_plugin(option) == "":
            weechat.config_set_plugin(option, default_value)
    weechat.hook_info("go_running", "Return '1' if go is running", "", "info_go_running", "")

def info_go_running(data, info_name, arguments):
    global hooks
    if "modifier" in hooks:
        return "1"
    return "0"

def unhook_one(hook):
    """ Unhook something hooked by this script """
    global hooks
    if hook in hooks:
        weechat.unhook(hooks[hook])
        del hooks[hook]

def unhook_all():
    """ Unhook all """
    global hook_command_run
    unhook_one("modifier")
    for hook in hook_command_run:
        unhook_one(hook)

def hook_all():
    """ Hook command_run and modifier """
    global hook_command_run, hooks
    priority = ""
    version = weechat.info_get("version_number", "") or 0
    # use high priority for hook to prevent conflict with other plugins/scripts
    # (WeeChat >= 0.3.4 only)
    if int(version) >= 0x00030400:
        priority = "2000|"
    for hook, value in hook_command_run.items():
        if hook not in hooks:
            hooks[hook] = weechat.hook_command_run("%s%s" % (priority, value[0]),
                                                   value[1], "")
    if "modifier" not in hooks:
        hooks["modifier"] = weechat.hook_modifier(
            "input_text_display_with_cursor", "input_modifier", "")

def go_start(buffer):
    """ Start go on buffer """
    global saved_input, saved_input_pos, old_input, buffers_pos
    hook_all()
    saved_input = weechat.buffer_get_string(buffer, "input")
    saved_input_pos = weechat.buffer_get_integer(buffer, "input_pos")
    weechat.buffer_set(buffer, "input", "")
    old_input = None
    buffers_pos = 0

def go_end(buffer):
    """ End go on buffer """
    global saved_input, saved_input_pos, old_input
    unhook_all()
    weechat.buffer_set(buffer, "input", saved_input)
    weechat.buffer_set(buffer, "input_pos", str(saved_input_pos))
    old_input = None

def go_now(buffer, args):
    """ Go to buffer specified by args """
    buffers = get_matching_buffers(args)
    # Prefer buffer that matches at beginning
    for index in range(len(buffers)):
        if re.search(r"^#?" + re.escape(args), buffers[index]["name"]):
            weechat.command(buffer, "/buffer " + str(buffers[index]["number"]))
            return None
    # Otherwise, just take first buffer in list
    if len(buffers) > 0:
        weechat.command(buffer, "/buffer " + str(buffers[0]["number"]))

def go_cmd(data, buffer, args):
    """ Command "/go": just hook what we need """
    global hooks
    if args:
        go_now(buffer, args)
    elif "modifier" in hooks:
        go_end(buffer)
    else:
        go_start(buffer)
    return weechat.WEECHAT_RC_OK

def get_matching_buffers(input):
    """ Return list with buffers matching user input """
    global buffers_pos
    list = []
    if len(input) == 0:
        buffers_pos = 0
    input = input.lower()
    infolist = weechat.infolist_get("buffer", "", "")
    while weechat.infolist_next(infolist):
        if weechat.config_get_plugin("short_name") == "on":
            name = weechat.infolist_string(infolist, "short_name")
        else:
            name = weechat.infolist_string(infolist, "name")
        if weechat.config_get_plugin("use_core_instead_weechat") == "on" and name == "weechat":
            name = "core"
        number = weechat.infolist_integer(infolist, "number")
        pointer = weechat.infolist_pointer(infolist, "pointer")
        matching = name.lower().find(input) >= 0
        if not matching and input[-1] == ' ':
            matching = name.lower().endswith(input.strip())
        if not matching and input.isdigit():
            matching = str(number).startswith(input)
        if len(input) == 0 or matching:
            list.append({"number": number, "name": name, "pointer": pointer})
            if len(input) == 0 and pointer == weechat.current_buffer():
                buffers_pos = len(list) - 1
    weechat.infolist_free(infolist)
    if not weechat.config_string_to_boolean(weechat.config_get_plugin('sort_by_activity')):
        return list
    # sort buffers like in hotlist.
    hotlist = []
    infolist = weechat.infolist_get("hotlist", "", "")
    while weechat.infolist_next(infolist):
        hotlist.append(weechat.infolist_pointer(infolist, "buffer_pointer"))
    weechat.infolist_free(infolist)
    last_index = len(hotlist)
    def priority(b):
        try:
            return hotlist.index(b["pointer"])
        except ValueError:
            # not in hotlist, always last.
            return last_index
    return sorted(list, key=priority)

def buffers_to_string(buffers, pos, input):
    """ Return string built using list of buffers found (matching user input) """
    global settings
    string = ""
    colors = {}
    input = input.lower()
    for option in settings:
        colors[option] = weechat.config_get_plugin(option)
    for i in range(len(buffers)):
        selected = ""
        if i == pos:
            selected = "_selected"
        index = buffers[i]["name"].lower().find(input)
        if index >= 0:
            index2 = index + len(input)
            string += " " + \
                weechat.color(colors["color_number" + selected]) + str(buffers[i]["number"]) + \
                weechat.color(colors["color_name" + selected]) + buffers[i]["name"][:index] + \
                weechat.color(colors["color_name_highlight" + selected]) + buffers[i]["name"][index:index2] + \
                weechat.color(colors["color_name" + selected]) + buffers[i]["name"][index2:] + \
                weechat.color("reset")
        else:
            string += " " + \
                weechat.color(colors["color_number" + selected]) + str(buffers[i]["number"]) + \
                weechat.color(colors["color_name" + selected]) + buffers[i]["name"] + \
                weechat.color("reset")
    if string != "":
        string = "  " + string
    return string

def input_modifier(data, modifier, modifier_data, string):
    """ This modifier is called when input text item is built by WeeChat
    (commonly after changes in input or cursor move), it builds new input with
    prefix ("Go to:"), and suffix (list of buffers found) """
    global old_input, buffers, buffers_pos
    if modifier_data != weechat.current_buffer():
        return ""
    names = ""
    input = weechat.string_remove_color(string, "")
    input = input.lstrip()
    if old_input == None or input != old_input:
        old_buffers = buffers
        buffers = get_matching_buffers(input)
        if buffers != old_buffers and len(input) > 0:
            buffers_pos = 0
        old_input = input
    names = buffers_to_string(buffers, buffers_pos, input.strip())
    return weechat.config_get_plugin("message") + string + names

def command_run_input(data, buffer, command):
    """ Function called when a command "/input xxxx" is run """
    global buffers, buffers_pos
    if command == "/input search_text" or command.find("/input jump") == 0:
        # search text or jump to another buffer is forbidden now
        return weechat.WEECHAT_RC_OK_EAT
    elif command == "/input complete_next":
        # choose next buffer in list
        buffers_pos += 1
        if buffers_pos >= len(buffers):
            buffers_pos = 0
        weechat.hook_signal_send("input_text_changed",
                                 weechat.WEECHAT_HOOK_SIGNAL_STRING, "")
        return weechat.WEECHAT_RC_OK_EAT
    elif command == "/input complete_previous":
        # choose previous buffer in list
        buffers_pos -= 1
        if buffers_pos < 0:
            buffers_pos = len(buffers) - 1
        weechat.hook_signal_send("input_text_changed",
                                 weechat.WEECHAT_HOOK_SIGNAL_STRING, "")
        return weechat.WEECHAT_RC_OK_EAT
    elif command == "/input return":
        # switch to selected buffer (if any)
        go_end(buffer)
        if len(buffers) > 0:
            weechat.command(buffer, "/buffer " + str(buffers[buffers_pos]["number"]))
        return weechat.WEECHAT_RC_OK_EAT
    return weechat.WEECHAT_RC_OK

def command_run_buffer(data, buffer, command):
    """ Function called when a command "/buffer xxxx" is run """
    return weechat.WEECHAT_RC_OK_EAT

def command_run_window(data, buffer, command):
    """ Function called when a command "/buffer xxxx" is run """
    return weechat.WEECHAT_RC_OK_EAT

def go_unload_script():
    """ Function called when script is unloaded """
    unhook_all()
    return weechat.WEECHAT_RC_OK
