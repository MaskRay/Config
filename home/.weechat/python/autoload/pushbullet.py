# -*- coding:utf-8 -*-
# pushbullet
#
# Adapted from pushover.pl by stfn <stfnmd@gmail.com>

import weechat as w

import json, time, urllib2

SCRIPT_NAME = 'pushbullet'
SCRIPT_AUTHOR = 'MaskRay'
SCRIPT_DESC = 'Send notifications to pushbullet.com on `notify_private,notify_highlight`'
SCRIPT_VERSION = '0.0.0'
SCRIPT_LICENSE = 'GPL3'

DEFAULTS = {
    'access_token': ('', 'Access Token obtained at https://www.pushbullet.com/#settings/account'),
    'blacklist': ('', 'Comma separated list of buffers (`buffer_match_list`) to blacklist for notifications'),
    'rate_limit': ('0', 'Rate limit in seconds (0 = unlimited), will send a maximum of 1 notification per time limit'),
    'show_highlight': ('on', 'Notify on `notify_highlight`'),
}
CONFIG = {}
last = {}


def parse_config():
    for option, (default, desc) in DEFAULTS.items():
        if not w.config_is_set_plugin(option):
            w.config_set_plugin(option, default)
        w.config_set_desc_plugin(option, '{} (default: {!r})'.format(desc, default))
    CONFIG['rate_limit'] = int(w.config_get_plugin('rate_limit'))
    for i in ('access_token', 'blacklist', 'show_highlight'):
        CONFIG[i] = w.config_get_plugin(i)
    return w.WEECHAT_RC_OK


def config_cb(data, option, value):
    parse_config()
    return w.WEECHAT_RC_OK


def print_cb(data, buffer, date, tags, displayed, highlight, prefix, message):
    global last
    is_private = w.buffer_get_string(buffer, 'localvar_type') == 'private'
    if (is_private or CONFIG['show_highlight'] == 'on' and highlight == 1) and \
            (buffer not in last or time.time() - last[buffer] > CONFIG['rate_limit']) and \
            not w.buffer_match_list(buffer, CONFIG['blacklist']):
        parsed = w.info_get_hashtable('irc_message_parse', {'message': message})
        try:
            body = '<{}> {}'.format(prefix, message)
            if not is_private:
                body = '[{}] {}'.format(w.buffer_get_string(buffer, 'short_name'), body)
            data = json.dumps({
                'type': 'note',
                'title': 'weechat',
                'body': body,
            })
            if CONFIG['access_token']:
                #w.prnt(buffer, '{}'.format(body))
                urllib2.urlopen(urllib2.Request('https://api.pushbullet.com/v2/pushes', data=data, headers={'Access-Token': CONFIG['access_token'], 'Content-type': 'application/json'}))
            else:
                w.prnt('', 'Please get an Access Token at https://www.pushbullet.com/#settings/account and /set plugins.var.python.'+SCRIPT_NAME+'.access_token')
        except:
            return w.WEECHAT_RC_ERROR
        else:
            last[buffer] = time.time()
    return w.WEECHAT_RC_OK


def mark_read_cb(_data, _signal, _signal_data):
    buffer = w.buffer_get_string(w.current_buffer(), 'name')
    last[buffer] = time.time()
    return w.WEECHAT_RC_OK


if __name__ == '__main__':
    w.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
               SCRIPT_DESC, '', '')
    parse_config()
    w.hook_config('plugins.var.python.'+SCRIPT_NAME+'.*', 'config_cb', '')
    w.hook_print('', 'notify_private,notify_highlight', '', 1, 'print_cb', '')
    w.hook_signal('*,irc_out1_privmsg', 'mark_read_cb', '')
    w.hook_signal('buffer_switch', 'mark_read_cb', '')
