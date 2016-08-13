# -*- coding: utf-8 -*-
#
# Copyright (c) 2011-2013 by F. Besser <fbesser@gmail.com>
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
# History:
# 2013-09-01, nils_2@freenode.#weechat:
#     version 0.2: add support of servername for "-exclude"
#                : make script behave like /allchan and /allserver command
#                : add function "-current"
#                : case-insensitive search for query/server
#
# 2011-09-05, F. Besser <fbesser@gmail.com>:
#     version 0.1: script created
#
# Development is on:
# https://github.com/fbesser/weechat_scripts
#
# (this script requires WeeChat 0.3.0 or newer)
#


SCRIPT_NAME = "allquery"
SCRIPT_AUTHOR = "fbesser"
SCRIPT_VERSION = "0.2"
SCRIPT_LICENSE = "GPL3"
SCRIPT_DESC = "Executes command on all irc query buffer"

SCRIPT_COMMAND = "allquery"

import_ok = True

try:
    import weechat
except ImportError:
    print('This script must be run under WeeChat.')
    print('Get WeeChat now at: http://www.weechat.org/')
    import_ok = False

try:
    import re
except ImportError, message:
    print('Missing package(s) for %s: %s' % (SCRIPT_NAME, message))
    import_ok = False


def make_list(argument):
    """ Make a list out of argument string of format -argument=value0,value1"""
    arglist = argument.lower().split("=", 1)
    arguments = arglist[1].split(",")
    return arguments

def allquery_command_cb(data, buffer, args):
    """ Callback for /allquery command """
    args = args.strip()
    if args == "":
        weechat.command("", "/help %s" % SCRIPT_COMMAND)
        return weechat.WEECHAT_RC_OK
    argv = args.split(" ")

    exclude_nick = None
    current_server = None

    if '-current' in argv:
        current_server = weechat.buffer_get_string(weechat.current_buffer(), "localvar_server")
        # remove "-current" + whitespace from argumentlist
        args = args.replace("-current", "")
        args = args.lstrip()
        argv.remove("-current")

    # search for "-exclude" in arguments
    i = 0
    for entry in argv[0:]:
        if entry.startswith("-exclude="):
            exclude_nick = make_list(argv[i])
            command = " ".join(argv[i+1::])
            break
        i +=1
    else:
        command = args

    # no command found.
    if not command:
        return weechat.WEECHAT_RC_OK

    if not command.startswith("/"):
        command = "/%s" % command

    infolist = weechat.infolist_get("buffer", "", "")
    while weechat.infolist_next(infolist):
        if weechat.infolist_string(infolist, "plugin_name") == "irc":
            ptr = weechat.infolist_pointer(infolist, "pointer")
            server = weechat.buffer_get_string(ptr, "localvar_server")
            query = weechat.buffer_get_string(ptr, "localvar_channel")
            execute_command = re.sub(r'\$nick', query, command)
            if weechat.buffer_get_string(ptr, "localvar_type") == "private":
                if current_server is not None:
                    if server == current_server:
                        exclude_nick_and_server(ptr,query,server,exclude_nick,execute_command)
                else:
                    exclude_nick_and_server(ptr,query,server,exclude_nick,execute_command)
    weechat.infolist_free(infolist)
    return weechat.WEECHAT_RC_OK


def exclude_nick_and_server(ptr, query, server, exclude_nick, execute_command):
    server = "%s.*" % server            # servername + ".*"
    if exclude_nick is not None:
        if not query.lower() in exclude_nick and not server.lower() in exclude_nick:
            weechat.command(ptr, execute_command)
    else:
        weechat.command(ptr, execute_command)


if __name__ == '__main__' and import_ok:
    if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION,
                        SCRIPT_LICENSE, SCRIPT_DESC, "", ""):

        weechat.hook_command(SCRIPT_COMMAND, SCRIPT_DESC,
                             '[-current] [-exclude=<nick|server>[,<nick2|server>...]] <command> [<arguments>]',
                             '   -current: execute command for query of current server only\n'
                             '   -exclude: exclude some querys and/or server from executed command\n'
                             '    command: command executed in query buffers\n'
                             '  arguments: arguments for command (special variables $nick will be replaced by its value)\n\n'
                             'Examples:\n'
                             '  close all query buffers:\n'
                             '    /' + SCRIPT_COMMAND + ' buffer close\n'
                             '  close all query buffers, but don\'t close FlashCode:\n'
                             '    /' + SCRIPT_COMMAND + ' -exclude=FlashCode buffer close\n'
                             '  close all query buffers, except for server freenode:\n'
                             '    /' + SCRIPT_COMMAND + ' -exclude=freenode.* buffer close\n'
                             '  msg to all query buffers:\n'
                             '    /' + SCRIPT_COMMAND + ' say Hello\n'
                             '  notice to all query buffers:\n'
                             '    /' + SCRIPT_COMMAND + ' notice $nick Hello',
                             '%(commands)',
                             'allquery_command_cb', '')
