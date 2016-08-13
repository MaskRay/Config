# -*- coding: utf-8 -*-
# =============================================================================
#  shell.py (c) March 2006, 2009 by Kolter <kolter@openics.org>
#
#  Licence     : GPL v2
#  Description : running shell commands in WeeChat
#  Syntax      : try /help shell to get some help on this script
#  Precond     : needs weechat >= 0.3.0 to run
#
#
# ### changelog ###
#
#  * version 0.8, 2013-07-27, Sebastien Helleu <flashcode@flashtux.org>:
#      - don't remove empty lines in output of command
#  * version 0.7, 2012-11-26, Sebastien Helleu <flashcode@flashtux.org>:
#      - use hashtable for command arguments (for WeeChat >= 0.4.0)
#  * version 0.6, 2012-11-21, Sebastien Helleu <flashcode@flashtux.org>:
#      - call shell in hook_process (WeeChat >= 0.3.9.2 does not call shell any more)
#  * version 0.5, 2011-10-01, Sebastien Helleu <flashcode@flashtux.org>:
#      - add shell buffer
#  * version 0.4, 2009-05-02, Sebastien Helleu <flashcode@flashtux.org>:
#      - sync with last API changes
#  * version 0.3, 2009-03-06, Sebastien Helleu <flashcode@flashtux.org>:
#      - use of hook_process to run background process
#      - add option -t <timeout> to kill process after <timeout> seconds
#      - show process running, kill it with -kill
#  * version 0.2, 2009-01-31, Sebastien Helleu <flashcode@flashtux.org>:
#      - conversion to WeeChat 0.3.0+
#  * version 0.1, 2006-03-13, Kolter <kolter@openics.org>:
#      - first release
#
# =============================================================================

import weechat, os, datetime

SCRIPT_NAME    = 'shell'
SCRIPT_AUTHOR  = 'Kolter'
SCRIPT_VERSION = '0.8'
SCRIPT_LICENSE = 'GPL2'
SCRIPT_DESC    = 'Run shell commands in WeeChat'

SHELL_CMD      = 'shell'
SHELL_PREFIX   = '[shell] '

cmd_hook_process   = ''
cmd_command        = ''
cmd_start_time     = None
cmd_buffer         = ''
cmd_shell_buffer   = ''
cmd_stdout         = ''
cmd_stderr         = ''
cmd_send_to_buffer = ''
cmd_timeout        = 0

if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
                    SCRIPT_DESC, '', ''):
    weechat.hook_command(
        SHELL_CMD,
        'Running shell commands in WeeChat',
        '[-o|-n] [-t seconds] <command> || -show || -kill',
        '        -o: send output to current buffer (simulate user entry '
        'with command output - dangerous, be careful when using this option)\n'
        '        -n: display output in a new empty buffer\n'
        '-t seconds: auto-kill process after timeout (seconds) if process '
        'is still running\n'
        '   command: shell command or builtin like cd, getenv, setenv, unsetenv\n'
        '     -show: show running process\n'
        '     -kill: kill running process',
        '-o|-n|-t|cd|getenv|setenv|unsetenv|-show||-kill -o|-n|-t|cd|getenv|setenv|unsetenv',
        'shell_cmd', '')

def shell_init():
    """Initialize some variables."""
    global cmd_hook_process, cmd_command, cmd_start_time, cmd_buffer, cmd_stdout, cmd_stderr
    cmd_hook_process = ''
    cmd_command      = ''
    cmd_start_time   = None
    cmd_buffer       = ''
    cmd_stdout       = ''
    cmd_stderr       = ''

def shell_set_title():
    """Set title on shell buffer (with working directory)."""
    global cmd_shell_buffer
    if cmd_shell_buffer:
        weechat.buffer_set(cmd_shell_buffer, 'title',
                           '%s.py %s | "q": close buffer | Working dir: %s' % (SCRIPT_NAME, SCRIPT_VERSION, os.getcwd()))

def shell_process_cb(data, command, rc, stdout, stderr):
    """Callback for hook_process()."""
    global cmd_hook_process, cmd_buffer, cmd_stdout, cmd_stderr, cmd_send_to_buffer
    cmd_stdout += stdout
    cmd_stderr += stderr
    if int(rc) >= 0:
        if cmd_stdout:
            lines = cmd_stdout.rstrip().split('\n')
            if cmd_send_to_buffer == 'current':
                for line in lines:
                    weechat.command(cmd_buffer, '%s' % line)
            else:
                weechat.prnt(cmd_buffer, '')
                if cmd_send_to_buffer != 'new':
                    weechat.prnt(cmd_buffer, '%sCommand "%s" (rc %d), stdout:'
                                 % (SHELL_PREFIX, data, int(rc)))
                for line in lines:
                    weechat.prnt(cmd_buffer, ' \t%s' % line)
        if cmd_stderr:
            lines = cmd_stderr.rstrip().split('\n')
            if cmd_send_to_buffer == 'current':
                for line in lines:
                    weechat.command(cmd_buffer, '%s' % line)
            else:
                weechat.prnt(cmd_buffer, '')
                if cmd_send_to_buffer != 'new':
                    weechat.prnt(cmd_buffer, '%s%sCommand "%s" (rc %d), stderr:'
                                 % (weechat.prefix('error'), SHELL_PREFIX, data, int(rc)))
                for line in lines:
                    weechat.prnt(cmd_buffer, '%s%s' % (weechat.prefix('error'), line))
        cmd_hook_process = ''
        shell_set_title()
    return weechat.WEECHAT_RC_OK

def shell_show_process(buffer):
    """Show running process."""
    global cmd_command, cmd_start_time
    if cmd_hook_process:
        weechat.prnt(buffer, '%sprocess running: "%s" (started on %s)'
                     % (SHELL_PREFIX, cmd_command, cmd_start_time.ctime()))
    else:
        weechat.prnt(buffer, '%sno process running' % SHELL_PREFIX)

def shell_kill_process(buffer):
    """Kill running process."""
    global cmd_hook_process, cmd_command
    if cmd_hook_process:
        weechat.unhook(cmd_hook_process)
        weechat.prnt(buffer, '%sprocess killed (command "%s")' % (SHELL_PREFIX, cmd_command))
        shell_init()
    else:
        weechat.prnt(buffer, '%sno process running' % SHELL_PREFIX)

def shell_chdir(buffer, directory):
    """Change working directory."""
    if not directory:
        if os.environ.has_key('HOME'):
            directory = os.environ['HOME']
    try:
        os.chdir(directory)
    except:
        weechat.prnt(buffer, '%san error occured while running command "cd %s"' % (SHELL_PREFIX, directory))
    else:
        weechat.prnt(buffer, '%schdir to "%s" ok, new path: %s' % (SHELL_PREFIX, directory, os.getcwd()))
    shell_set_title()

def shell_getenv(buffer, var):
    """Get environment variable."""
    global cmd_send_to_buffer
    var = var.strip()
    if not var:
        weechat.prnt(buffer, '%swrong syntax, try "getenv VAR"' % (SHELL_PREFIX))
        return

    value = os.getenv(var)
    if value == None:
        weechat.prnt(buffer, '%s$%s is not set' % (SHELL_PREFIX, var))
    else:
        if cmd_send_to_buffer == 'current':
            weechat.command(buffer, '$%s=%s' % (var, os.getenv(var)))
        else:
            weechat.prnt(buffer, '%s$%s=%s' % (SHELL_PREFIX, var, os.getenv(var)))

def shell_setenv(buffer, expr):
    """Set an environment variable."""
    global cmd_send_to_buffer
    expr = expr.strip()
    lexpr = expr.split('=')

    if (len(lexpr) < 2):
        weechat.prnt(buffer, '%swrong syntax, try "setenv VAR=VALUE"' % (SHELL_PREFIX))
        return

    os.environ[lexpr[0].strip()] = '='.join(lexpr[1:])
    if cmd_send_to_buffer != 'current':
        weechat.prnt(buffer, '%s$%s is now set to "%s"' % (SHELL_PREFIX, lexpr[0], '='.join(lexpr[1:])))

def shell_unsetenv(buffer, var):
    """Remove environment variable."""
    var = var.strip()
    if not var:
        weechat.prnt(buffer, '%swrong syntax, try "unsetenv VAR"' % (SHELL_PREFIX))
        return

    if os.environ.has_key(var):
        del os.environ[var]
        weechat.prnt(buffer, '%s$%s is now unset' % (SHELL_PREFIX, var))
    else:
        weechat.prnt(buffer, '%s$%s is not set' % (SHELL_PREFIX, var))

def shell_exec(buffer, command):
    """Execute a command."""
    global cmd_hook_process, cmd_command, cmd_start_time, cmd_buffer
    global cmd_stdout, cmd_stderr, cmd_send_to_buffer, cmd_timeout
    if cmd_hook_process:
        weechat.prnt(buffer,
                     '%sanother process is running! (use "/%s -kill" to kill it)'
                     % (SHELL_PREFIX, SHELL_CMD))
        return
    if cmd_send_to_buffer == 'new':
        weechat.prnt(buffer, '-->\t%s%s$ %s%s'
                     % (weechat.color('chat_buffer'), os.getcwd(), weechat.color('reset'), command))
        weechat.prnt(buffer, '')
    args = command.split(' ')
    if args[0] == 'cd':
        shell_chdir(buffer, ' '.join(args[1:]))
    elif args[0] == 'getenv':
        shell_getenv (buffer, ' '.join(args[1:]))
    elif args[0] == 'setenv':
        shell_setenv (buffer, ' '.join(args[1:]))
    elif args[0] == 'unsetenv':
        shell_unsetenv (buffer, ' '.join(args[1:]))
    else:
        shell_init()
        cmd_command = command
        cmd_start_time = datetime.datetime.now()
        cmd_buffer = buffer
        version = weechat.info_get("version_number", "") or 0
        if int(version) >= 0x00040000:
            cmd_hook_process = weechat.hook_process_hashtable('sh', { 'arg1': '-c', 'arg2': command },
                                                              cmd_timeout * 1000, 'shell_process_cb', command)
        else:
            cmd_hook_process = weechat.hook_process("sh -c '%s'" % command, cmd_timeout * 1000, 'shell_process_cb', command)

def shell_input_buffer(data, buffer, input):
    """Input callback on shell buffer."""
    global cmd_send_to_buffer
    if input in ('q', 'Q'):
        weechat.buffer_close(buffer)
        return weechat.WEECHAT_RC_OK
    cmd_send_to_buffer = 'new'
    weechat.prnt(buffer, '')
    command = weechat.string_input_for_buffer(input)
    shell_exec(buffer, command)
    return weechat.WEECHAT_RC_OK

def shell_close_buffer(data, buffer):
    """Close callback on shell buffer."""
    global cmd_shell_buffer
    cmd_shell_buffer = ''
    return weechat.WEECHAT_RC_OK

def shell_new_buffer():
    """Create shell buffer."""
    global cmd_shell_buffer
    cmd_shell_buffer = weechat.buffer_search('python', 'shell')
    if not cmd_shell_buffer:
        cmd_shell_buffer = weechat.buffer_new('shell', 'shell_input_buffer', '', 'shell_close_buffer', '')
    if cmd_shell_buffer:
        shell_set_title()
        weechat.buffer_set(cmd_shell_buffer, 'localvar_set_no_log', '1')
        weechat.buffer_set(cmd_shell_buffer, 'time_for_each_line', '0')
        weechat.buffer_set(cmd_shell_buffer, 'input_get_unknown_commands', '1')
        weechat.buffer_set(cmd_shell_buffer, 'display', '1')
    return cmd_shell_buffer

def shell_cmd(data, buffer, args):
    """Callback for /shell command."""
    global cmd_send_to_buffer, cmd_timeout
    largs = args.split(' ')

    # strip spaces
    while '' in largs:
        largs.remove('')
    while ' ' in largs:
        largs.remove(' ')

    cmdbuf = buffer

    if len(largs) ==  0:
        shell_new_buffer()
    else:
        if largs[0] == '-show':
            shell_show_process(cmdbuf)
        elif largs[0] == '-kill':
            shell_kill_process(cmdbuf)
        else:
            cmd_send_to_buffer = ''
            cmd_timeout = 0
            while largs:
                if largs[0] == '-o':
                    cmd_send_to_buffer = 'current'
                    largs = largs[1:]
                    continue
                if largs[0] == '-n':
                    cmd_send_to_buffer = 'new'
                    cmdbuf = shell_new_buffer()
                    largs = largs[1:]
                    continue
                if largs[0] == '-t' and len(largs) > 2:
                    cmd_timeout = int(largs[1])
                    largs = largs[2:]
                    continue
                break
            if len(largs) > 0:
                shell_exec(cmdbuf, ' '.join(largs))
    return weechat.WEECHAT_RC_OK
