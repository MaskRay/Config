# -*- coding:utf-8 -*-
# pastebin

import collections, re, subprocess
import weechat as w

SCRIPT_NAME = 'pastebin'
SCRIPT_AUTHOR = 'MaskRay'
SCRIPT_DESC = '/paste /tmp/a.{jpg,txt} to send its pastebin/imagebin URL to the current buffer'
SCRIPT_VERSION = '0.0.0'
SCRIPT_LICENSE = 'GPLv3'
TIMEOUT = 10 * 1000

process_output = ''
buffers = collections.deque()

def pastebin_process_cb(data, command, rc, out, err):
    #w.prnt('', '{} {} {} {} {}'.format(data, command, rc, out, err))
    global process_output
    process_output += out
    if int(rc) >= 0:
        buffer = buffers.popleft()
        if re.match(r'^https?://', process_output):
            w.command(buffer, '/say '+process_output)
        else:
            w.prnt(buffer, str(process_output))
        process_output = ''
    return w.WEECHAT_RC_OK


def pastebin_cb(data, buffer, args):
    '''Callback for /paste command'''
    largs = args.split(' ')
    while '' in largs:
        largs.remove('')
    while ' ' in largs:
        largs.remove(' ')
    if len(largs) == 0:
        pass
    else:
        filename = largs[0]
        if re.search(r'\.(bmp|jpe?g|gif|png|webp)$', filename):
            cmd = 'curl -sF "name=@{}" http://img.vim-cn.com/'.format(filename)
        else:
            cmd = 'curl -sF "vimcn=<{}" http://cfp.vim-cn.com/'.format(filename)
        buffers.append(buffer)
        w.hook_process(cmd, TIMEOUT, pastebin_process_cb.__name__, '')
    return w.WEECHAT_RC_OK


if w.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
              SCRIPT_DESC, '', ''):
    w.hook_command('paste',
                   'Send the pastebin/imagebin URL of a local file to the current buffer',
                   '<file>',
                   '',
                   '',
                   pastebin_cb.__name__, '')
