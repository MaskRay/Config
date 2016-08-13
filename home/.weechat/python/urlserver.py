# -*- coding: utf-8 -*-
#
# Copyright (C) 2011-2015 Sébastien Helleu <flashcode@flashtux.org>
# Copyright (C) 2011 xt <xt@bash.no>
# Copyright (C) 2012 Filip H.F. "FiXato" Slagter
#                    <fixato+weechat+urlserver@gmail.com>
# Copyright (C) 2012 WillyKaze <willykaze@willykaze.org>
# Copyright (C) 2013 Thomas Kindler <mail_weechat@t-kindler.de>
# Copyright (C) 2013 Felix Eckhofer <felix@tribut.de>
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
# Shorten URLs with own HTTP server.
# (this script requires Python >= 2.6)
#
# How does it work?
#
# 1. The URLs displayed in buffers are shortened and stored in memory (saved in
#    a file when script is unloaded).
# 2. URLs shortened can be displayed below messages, in a dedicated buffer, or
#    as HTML page in your browser.
# 3. This script embeds an HTTP server, which will redirect shortened URLs
#    to real URL and display list of all URLs if you browse address without
#    URL key.
# 4. It is recommended to customize/protect the HTTP server using script
#    options (see /help urlserver).
#
# List of URLs:
# - in WeeChat: /urlserver
# - in browser: http://myhost.org:1234/
#
# History:
#
# 2016-01-20, Yves Stadler <yves.stadler@gmail.com>:
#     v2.0: add option "http_open_in_new_page"
# 2015-05-16, Sébastien Helleu <flashcode@flashtux.org>:
#     v1.9: add option "http_auth_redirect", fix flake8 warnings
# 2015-04-14, Sébastien Helleu <flashcode@flashtux.org>:
#     v1.8: evaluate option "http_auth" (to use secured data)
# 2013-12-09, WakiMiko
#     v1.7: use HTTPS for youtube embedding
# 2013-12-09, Sébastien Helleu <flashcode@flashtux.org>:
#     v1.6: add reason phrase after HTTP code 302 and empty line at the end
# 2013-12-05, Sébastien Helleu <flashcode@flashtux.org>:
#     v1.5: replace HTTP 301 by 302
# 2013-12-05, Sébastien Helleu <flashcode@flashtux.org>:
#     v1.4: use HTTP 301 instead of meta for the redirection when
#           there is no referer in request
# 2013-11-29, Felix Eckhofer <felix@tribut.de>
#     v1.3: - make it possible to run reverse proxy in a subdirectory by
#             generating relative links and using the <base> tag. to use this,
#             set http_hostname_display to 'domain.tld/subdir'.
#           - mention favicon explicitly (now works in subdirectories, too).
#           - update favicon to new weechat logo.
#           - set meta referrer to never in redirect page, so chrome users'
#             referrers are hidden, too
#           - fix http_auth in chrome and other browsers which send header
#             names in lower case
# 2013-05-04, Thomas Kindler <mail_weechat@t-kindler.de>
#     v1.2: added a "http_scheme_display" option. This makes it possible to run
#           the server behind a reverse proxy with https:// URLs.
# 2013-03-25, Hermit (@irc.freenode.net):
#     v1.1: made links relative in the html, so that they can be followed when
#           accessing the listing remotely using the weechat box's IP directly.
# 2012-12-12, WillyKaze <willykaze@willykaze.org>:
#     v1.0: add options "http_time_format", "display_msg_in_url" (works with
#           relay/irc), "color_in_msg", "separators"
# 2012-04-18, Filip H.F. "FiXato" Slagter <fixato+weechat+urlserver@gmail.com>:
#     v0.9: add options "http_autostart", "http_port_display"
#           "url_min_length" can now be set to -1 to auto-detect minimal url
#           length; also, if port is 80 now, :80 will no longer be added to the
#           shortened url.
# 2012-04-17, Filip H.F. "FiXato" Slagter <fixato+weechat+urlserver@gmail.com>:
#     v0.8: add more CSS support by adding options "http_fg_color",
#           "http_css_url" and "http_title", add descriptive classes to most
#           html elements.
# 2012-04-11, Sébastien Helleu <flashcode@flashtux.org>:
#     v0.7: fix truncated HTML page (thanks to xt), fix base64 decoding with
#           Python 3.x
# 2012-01-19, Sébastien Helleu <flashcode@flashtux.org>:
#     v0.6: add option "http_hostname_display"
# 2012-01-03, Sébastien Helleu <flashcode@flashtux.org>:
#     v0.5: make script compatible with Python 3.x
# 2011-10-31, Sébastien Helleu <flashcode@flashtux.org>:
#     v0.4: add options "http_embed_youtube_size" and "http_bg_color",
#           add extensions jpeg/bmp/svg for embedded images
# 2011-10-30, Sébastien Helleu <flashcode@flashtux.org>:
#     v0.3: escape HTML chars for page with list of URLs, add option
#           "http_prefix_suffix", disable highlights on urlserver buffer
# 2011-10-30, Sébastien Helleu <flashcode@flashtux.org>:
#     v0.2: fix error on loading of file "urlserver_list.txt" when it is empty
# 2011-10-30, Sébastien Helleu <flashcode@flashtux.org>:
#     v0.1: initial release
#

SCRIPT_NAME = 'urlserver'
SCRIPT_AUTHOR = 'Sébastien Helleu <flashcode@flashtux.org>'
SCRIPT_VERSION = '2.0'
SCRIPT_LICENSE = 'GPL3'
SCRIPT_DESC = 'Shorten URLs with own HTTP server'

SCRIPT_COMMAND = 'urlserver'
SCRIPT_BUFFER = 'urlserver'

import_ok = True

try:
    import weechat
except ImportError:
    print('This script must be run under WeeChat.')
    print('Get WeeChat now at: http://www.weechat.org/')
    import_ok = False

try:
    import ast
    import base64
    import cgi
    import datetime
    import os
    import re
    import socket
    import string
    import sys
except ImportError as message:
    print('Missing package(s) for %s: %s' % (SCRIPT_NAME, message))
    import_ok = False

# regex are from urlbar.py, written by xt
url_octet = r'(?:2(?:[0-4]\d|5[0-5])|1\d\d|\d{1,2})'
url_ipaddr = r'%s(?:\.%s){3}' % (url_octet, url_octet)
url_label = r'[0-9a-z][-0-9a-z]*[0-9a-z]?'
url_domain = r'%s(?:\.%s)*\.[a-z][-0-9a-z]*[a-z]?' % (url_label, url_label)

urlserver = {
    'socket': None,
    'hook_fd': None,
    'regex': re.compile(r'((?:\w+:)+//(?:%s|%s)(?::\d+)?(?:/[^\])>\s]*)?)' %
                        (url_domain, url_ipaddr),
                        re.IGNORECASE),
    'urls': {},
    'number': 0,
    'buffer': '',
}

# script options
urlserver_settings_default = {
    # HTTP server settings
    'http_autostart': (
        'on',
        'start the built-in HTTP server automatically)'),
    'http_scheme_display': (
        'http',
        'display this scheme in shortened URLs'),
    'http_hostname': (
        '',
        'force hostname/IP in bind of socket '
        '(empty value = auto-detect current hostname)'),
    'http_hostname_display': (
        '',
        'display this hostname in shortened URLs'),
    'http_port': (
        '',
        'force port for listening (empty value = find a random free port)'),
    'http_port_display': (
        '',
        'display this port in shortened URLs. Useful if you forward '
        'a different external port to the internal port'),
    'http_allowed_ips': (
        '',
        'regex for IPs allowed to use server '
        '(example: "^(123.45.67.89|192.160.*)$")'),
    'http_auth': (
        '',
        'login and password (format: "login:password") required to access to '
        'page with list of URLs (note: content is evaluated, see /help eval)'),
    'http_auth_redirect': (
        'on',
        'require the login/password (if option "http_auth" is set) for URLs '
        'redirections'),
    'http_url_prefix': (
        '',
        'prefix to add in URLs to prevent external people to scan your URLs '
        '(for example: prefix "xx" will give URL: http://host.com:1234/xx/8)'),
    'http_bg_color': (
        '#f4f4f4',
        'background color for HTML page'),
    'http_fg_color': (
        '#000',
        'foreground color for HTML page'),
    'http_css_url': (
        '',
        'URL of external Cascading Style Sheet to add (BE CAREFUL: the HTTP '
        'referer will be sent to site hosting CSS file!) (empty value = use '
        'default embedded CSS)'),
    'http_embed_image': (
        'off',
        'embed images in HTML page (BE CAREFUL: the HTTP referer will be sent '
        'to site hosting image!)'),
    'http_embed_youtube': (
        'off',
        'embed youtube videos in HTML page (BE CAREFUL: the HTTP referer '
        'will be sent to youtube!)'),
    'http_embed_youtube_size': (
        '480*350',
        'size for embedded youtube video, format is "xxx*yyy"'),
    'http_prefix_suffix': (
        ' ',
        'suffix displayed between prefix and message in HTML page'),
    'http_title': (
        'WeeChat URLs',
        'title of the HTML page'),
    'http_time_format': (
        '%d/%m/%y %H:%M:%S',
        'time format in the HTML page'),
    'http_open_in_new_page': (
        'on',
        'open links in new pages/tabs'),
    # message filter settings
    'msg_ignore_buffers': (
        'core.weechat,python.grep',
        'comma-separated list (without spaces) of buffers to ignore '
        '(full name like "irc.freenode.#weechat")'),
    'msg_ignore_tags': (
        'irc_quit,irc_part,notify_none',
        'comma-separated list (without spaces) of tags (or beginning of tags) '
        'to ignore (for example, use "notify_none" to ignore self messages or '
        '"nick_weebot" to ignore messages from nick "weebot")'),
    'msg_require_tags': (
        'nick_',
        'comma-separated list (without spaces) of tags (or beginning of tags) '
        'required to shorten URLs (for example "nick_" to shorten URLs only '
        'in messages from other users)'),
    'msg_ignore_regex': (
        '',
        'ignore messages matching this regex'),
    'msg_ignore_dup_urls': (
        'off',
        'ignore duplicated URLs (do not add an URL in list if it is already)'),
    # display settings
    'color': (
        'darkgray',
        'color for urls displayed after message'),
    'color_in_msg': (
        '',
        'color for urls displayed inside irc message: it is a number '
        '(irc color) between 00 and 15 (see doc for a list of irc colors)'),
    'separators': (
        '[|]',
        'separators for short url list (string with exactly 3 chars)'),
    'display_urls': (
        'on',
        'display URLs below messages'),
    'display_urls_in_msg': (
        'off',
        'add shorten url next to the original url (only in IRC messages) '
        '(useful for urlserver behind relay/irc)'),
    'url_min_length': (
        '0',
        'minimum length for an URL to be shortened (0 = shorten all URLs, '
        '-1 = detect length based on shorten URL)'),
    'urls_amount': (
        '100',
        'number of URLs to keep in memory (and in file when script is not '
        'loaded)'),
    'buffer_short_name': (
        'off',
        'use buffer short name on dedicated buffer'),
    'debug': (
        'off',
        'print some debug messages'),
}
urlserver_settings = {}


def base62_encode(number):
    """Encode a number in base62 (all digits + a-z + A-Z)."""
    base62chars = string.digits + string.ascii_letters
    l = []
    while number > 0:
        remainder = number % 62
        number = number // 62
        l.insert(0, base62chars[remainder])
    return ''.join(l) or '0'


def base62_decode(str_value):
    """Decode a base62 string (all digits + a-z + A-Z) to a number."""
    base62chars = string.digits + string.ascii_letters
    return sum([base62chars.index(char) * (62 ** (len(str_value) - index - 1))
                for index, char in enumerate(str_value)])


def base64_decode(s):
    if sys.version_info >= (3,):
        # python 3.x
        return base64.b64decode(s.encode('utf-8'))
    else:
        # python 2.x
        return base64.b64decode(s)


def urlserver_get_base_url():
    """
    Return url with port number if != default port for the protocol,
    including prefix path.
    """
    global urlserver_settings

    scheme = urlserver_settings['http_scheme_display']
    hostname = (urlserver_settings['http_hostname_display'] or
                urlserver_settings['http_hostname'] or socket.getfqdn())

    # If the built-in HTTP server isn't running, default to port from settings
    port = urlserver_settings['http_port']
    if len(urlserver_settings['http_port_display']) > 0:
        port = urlserver_settings['http_port_display']
    elif urlserver['socket']:
        port = urlserver['socket'].getsockname()[1]

    # Don't add :port if the port matches the default port for the protocol
    prefixed_port = ':%s' % port

    if scheme == "http" and prefixed_port == ':80':
        prefixed_port = ''
    elif scheme == "https" and prefixed_port == ':443':
        prefixed_port = ''

    prefix = ''
    if urlserver_settings['http_url_prefix']:
        prefix = '%s/' % urlserver_settings['http_url_prefix']

    return '%s://%s%s/%s' % (scheme, hostname, prefixed_port, prefix)


def urlserver_short_url(number, full=True):
    """Return short URL with number."""
    return '%s%s' % (urlserver_get_base_url() if full else '',
                     base62_encode(number))


def urlserver_server_reply(conn, code, extra_header, message,
                           mimetype='text/html'):
    """Send a HTTP reply to client."""
    global urlserver_settings
    if extra_header:
        extra_header += '\r\n'
    s = 'HTTP/1.1 %s\r\n' \
        '%s' \
        'Content-Type: %s\r\n' \
        'Content-Length: %d\r\n' \
        '\r\n' \
        % (code, extra_header, mimetype, len(message))
    msg = None
    if sys.version_info >= (3,):
        # python 3.x
        if type(message) is bytes:
            msg = s.encode('utf-8') + message
        else:
            msg = s.encode('utf-8') + message.encode('utf-8')
    else:
        # python 2.x
        msg = s + message
    if urlserver_settings['debug'] == 'on':
        weechat.prnt('', 'urlserver: sending %d bytes' % len(msg))
    conn.sendall(msg)


def urlserver_server_reply_auth_required(conn):
    """Reply a 401 (authorization required)."""
    urlserver_server_reply(conn,
                           '401 Authorization required',
                           'WWW-Authenticate: Basic realm="%s"' % SCRIPT_NAME,
                           '')


def urlserver_server_favicon():
    """Return favicon for HTML page."""
    s = ('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'
         'TkSuQmCC')
    return base64_decode(s)


def urlserver_server_reply_list(conn, sort='-time'):
    """Send list of URLs as HTML page to client."""
    global urlserver, urlserver_settings
    content = '<div class="urls">\n<table id="urls_table">\n'
    if not sort.startswith('-'):
        sort = '+%s' % sort
    if sort[1:] == 'time':
        urls = sorted(urlserver['urls'].items())
    else:
        idx = ['time', 'nick', 'buffer'].index(sort[1:])
        urls = sorted(urlserver['urls'].items(),
                      key=lambda url: url[1][idx].lower())
    if sort.startswith('-'):
        urls.reverse()
    sortkey = {
        '-': ('', '&uarr;'),
        '+': ('-', '&darr;')
    }
    content += '  <tr>'
    for column, defaultsort in (('time', '-'), ('nick', ''), ('buffer', '')):
        if sort[1:] == column:
            content += ('<th class="sortable sorted_by %s_header">'
                        '<a href="sort=%s%s">%s</a> %s</th>' % (
                            column,
                            sortkey[sort[0]][0],
                            column, column.capitalize(),
                            sortkey[sort[0]][1]))
        else:
            content += ('<th class="sortable %s_header">'
                        '<a class="sort_link" href="sort=%s%s">%s</a></th>' % (
                            column,
                            defaultsort,
                            column,
                            column.capitalize()))
    content += '<th class="unsortable message_header">URLs</th>'
    content += '</tr>\n'
    for key, item in urls:
        content += '  <tr>'
        url = item[3]
        obj = ''
        message = (cgi.escape(item[4].replace(url, '\x01\x02\x03\x04'))
                   .split('\t', 1))
        message[0] = '<span class="prefix">%s</span>' % message[0]
        message[1] = '<span class="message">%s</span>' % message[1]

        strjoin = ('<span class="prefix_suffix"> %s </span>' %
                   urlserver_settings['http_prefix_suffix']
                   .replace(' ', '&nbsp;'))

        target = ''
        if urlserver_settings['http_open_in_new_page'] == 'on':
            target = ' target=_blank'

        message = strjoin.join(message).replace(
            '\x01\x02\x03\x04',
            '</span><a class="url" href="%s" title="%s"%s>%s'
            '</a><span class="message">' % (
                urlserver_short_url(key, False), url, target, url))
        if urlserver_settings['http_embed_image'] == 'on' and \
                url.lower().endswith(('.jpg', '.jpeg', '.png', '.gif',
                                      '.bmp', '.svg')):
            obj = ('<div class="obj"><img src="%s" title="%s" alt="%s">'
                   '</div>' % (url, url, url))
        elif urlserver_settings['http_embed_youtube'] == 'on' and \
                'youtube.com/' in url:
            m = re.search('v=([\w\d]+)', url)
            if m:
                yid = m.group(1)
                try:
                    size = (urlserver_settings['http_embed_youtube_size']
                            .split('*'))
                    width = int(size[0])
                    height = int(size[1])
                except:
                    width = 480
                    height = 350
                obj = ('<div class="obj youtube">'
                       '<iframe id="%s" type="text/html" width="%d" '
                       'height="%d" '
                       'src="https://www.youtube.com/embed/%s?enablejsapi=1">'
                       '</iframe></div>' % (yid, width, height, yid))
        content += ('<td class="timestamp">%s</td>'
                    '<td class="nick">%s</td>'
                    '<td class="buffer">%s</td><td class="message">' % (
                        item[0], item[1], item[2]))
        content += '%s%s</td></tr>\n' % (message, obj)
    content += '</table>'
    if len(urlserver_settings['http_css_url']) > 0:
        css = ('<link rel="stylesheet" type="text/css" href="%s" />' %
               urlserver_settings['http_css_url'])
    else:
        css = ('<style type="text/css" media="screen">'
               '<!--\n'
               '  html { font-family: Verdana, Arial, Helvetica; '
               'font-size: 12px; background: %s; color: %s }\n'
               '  .urls table { border-collapse: collapse }\n'
               '  .urls table td,th { border: solid 1px #cccccc; '
               'padding: 4px; font-size: 12px }\n'
               '  .timestamp,.nick,.buffer { white-space: nowrap }\n'
               '  .sorted_by { font-style: italic; }\n'
               '  .obj { margin-top: 1em }\n'
               '-->'
               '</style>\n' % (
                   urlserver_settings['http_bg_color'],
                   urlserver_settings['http_fg_color']))

    html = ('<html>\n'
            '<head>\n'
            '<title>%s</title>\n'
            '<meta http-equiv="content-type" content="text/html; '
            'charset=utf-8" />\n'
            '%s\n'
            '<base href="%s" />\n'
            '<link rel="icon" type="image/png" href="favicon.png" />\n'
            '</head>\n'
            '<body>\n%s\n</body>\n'
            '</html>' % (
                urlserver_settings['http_title'],
                css,
                urlserver_get_base_url(),
                content))
    urlserver_server_reply(conn, '200 OK', '', html)


def urlserver_check_auth(data):
    """Check user/password to access a page/URL."""
    global urlserver_settings
    if not urlserver_settings['http_auth']:
        return True
    http_auth = weechat.string_eval_expression(
        urlserver_settings['http_auth'], {}, {}, {})
    auth = re.search('^Authorization: Basic (\S+)$', data,
                     re.MULTILINE | re.IGNORECASE)
    if auth and (base64_decode(auth.group(1)).decode('utf-8') == http_auth):
        return True
    return False


def urlserver_server_fd_cb(data, fd):
    """Callback for server socket."""
    global urlserver, urlserver_settings
    if not urlserver['socket']:
        return weechat.WEECHAT_RC_OK
    conn, addr = urlserver['socket'].accept()
    if urlserver_settings['debug'] == 'on':
        weechat.prnt('', 'urlserver: connection from %s' % str(addr))
    if urlserver_settings['http_allowed_ips'] and \
            not re.match(urlserver_settings['http_allowed_ips'], addr[0]):
        if urlserver_settings['debug'] == 'on':
            weechat.prnt('', 'urlserver: IP not allowed')
        conn.close()
        return weechat.WEECHAT_RC_OK
    data = None
    try:
        conn.settimeout(0.3)
        data = conn.recv(4096).decode('utf-8')
        data = data.replace('\r\n', '\n')
    except:
        return weechat.WEECHAT_RC_OK
    replysent = False
    sort = '-time'
    referer = re.search('^Referer:', data, re.MULTILINE | re.IGNORECASE)
    m = re.search('^GET /(.*) HTTP/.*$', data, re.MULTILINE)
    if m:
        url = m.group(1)
        if urlserver_settings['debug'] == 'on':
            weechat.prnt('', 'urlserver: %s' % m.group(0))
        if 'favicon.' in url:
            urlserver_server_reply(conn, '200 OK', '',
                                   urlserver_server_favicon(),
                                   mimetype='image/x-icon')
            replysent = True
        else:
            # check if prefix is ok (if prefix defined in settings)
            prefixok = True
            if urlserver_settings['http_url_prefix']:
                if url.startswith(urlserver_settings['http_url_prefix']):
                    url = url[len(urlserver_settings['http_url_prefix']):]
                    if url.startswith('/'):
                        url = url[1:]
                else:
                    prefixok = False
            # prefix ok, go on with url
            if prefixok:
                if url.startswith('sort='):
                    # sort asked for list of urls
                    sort = url[5:]
                    url = ''
                if url:
                    # short url, read base62 key and redirect to page
                    number = -1
                    try:
                        number = base62_decode(url)
                    except:
                        pass
                    if number >= 0 and number in urlserver['urls']:
                        authok = (
                            urlserver_settings['http_auth_redirect'] != 'on' or
                            urlserver_check_auth(data)
                        )
                        if authok:
                            # if we have a referer in request, use meta for
                            # redirection (so that referer is not sent)
                            # otherwise, we can make redirection with HTTP 302
                            if referer:
                                urlserver_server_reply(
                                    conn, '200 OK', '',
                                    '<meta name="referrer" content="never">\n'
                                    '<meta http-equiv="refresh" content="0; '
                                    'url=%s">' % urlserver['urls'][number][3])
                            else:
                                conn.sendall('HTTP/1.1 302\r\n'
                                             'Location: %s\r\n\r\n' %
                                             urlserver['urls'][number][3])
                        else:
                            urlserver_server_reply_auth_required(conn)
                        replysent = True
                else:
                    # page with list of urls
                    if urlserver_check_auth(data):
                        urlserver_server_reply_list(conn, sort)
                    else:
                        urlserver_server_reply_auth_required(conn)
                    replysent = True
            else:
                if urlserver_settings['debug'] == 'on':
                    weechat.prnt('', 'urlserver: prefix missing')
    if not replysent:
        urlserver_server_reply(conn,
                               '404 Not found', '',
                               '<html>\n'
                               '<head><title>Page not found</title></head>\n'
                               '<body><h1>Page not found</h1></body>\n'
                               '</html>')
    conn.close()
    return weechat.WEECHAT_RC_OK


def urlserver_server_status():
    """Display status of server."""
    global urlserver
    if urlserver['socket']:
        weechat.prnt('', 'URL server listening on %s' %
                     str(urlserver['socket'].getsockname()))
    else:
        weechat.prnt('', 'URL server not running')


def urlserver_server_start():
    """Start mini HTTP server."""
    global urlserver, urlserver_settings
    if urlserver['socket']:
        weechat.prnt('', 'URL server already running')
        return
    port = 0
    try:
        port = int(urlserver_settings['http_port'])
    except:
        port = 0
    urlserver['socket'] = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    urlserver['socket'].setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    try:
        urlserver['socket'].bind((urlserver_settings['http_hostname'] or
                                  socket.getfqdn(), port))
    except Exception as e:
        weechat.prnt('', '%sBind error: %s' % (weechat.prefix('error'), e))
        urlserver['socket'] = None
        urlserver_server_status()
        return
    urlserver['socket'].listen(5)
    urlserver['hook_fd'] = weechat.hook_fd(urlserver['socket'].fileno(),
                                           1, 0, 0,
                                           'urlserver_server_fd_cb', '')
    urlserver_server_status()


def urlserver_server_stop():
    """Stop mini HTTP server."""
    global urlserver
    if urlserver['socket'] or urlserver['hook_fd']:
        if urlserver['socket']:
            urlserver['socket'].close()
            urlserver['socket'] = None
        if urlserver['hook_fd']:
            weechat.unhook(urlserver['hook_fd'])
            urlserver['hook_fd'] = None
        weechat.prnt('', 'URL server stopped')


def urlserver_server_restart():
    """Restart mini HTTP server."""
    urlserver_server_stop()
    urlserver_server_start()


def urlserver_display_url_detail(key, return_url=False):
    global urlserver
    url = urlserver['urls'][key]
    nick = url[1]
    if nick:
        nick += ' @ '

    if return_url:
        return urlserver_short_url(key)
    else:
        weechat.prnt_date_tags(
            urlserver['buffer'], 0, 'notify_none',
            '%s, %s%s%s%s: %s%s%s -> %s' % (
                url[0],
                nick,
                weechat.color('chat_buffer'),
                url[2],
                weechat.color('reset'),
                weechat.color(urlserver_settings['color']),
                urlserver_short_url(key),
                weechat.color('reset'),
                url[3]))


def urlserver_buffer_input_cb(data, buffer, input_data):
    if input_data in ('q', 'Q'):
        weechat.buffer_close(buffer)
    return weechat.WEECHAT_RC_OK


def urlserver_buffer_close_cb(data, buffer):
    global urlserver
    urlserver['buffer'] = ''
    return weechat.WEECHAT_RC_OK


def urlserver_open_buffer():
    global urlserver, urlserver_settings
    if not urlserver['buffer']:
        urlserver['buffer'] = weechat.buffer_new(
            SCRIPT_BUFFER,
            'urlserver_buffer_input_cb', '',
            'urlserver_buffer_close_cb', '')
    if urlserver['buffer']:
        weechat.buffer_set(urlserver['buffer'], 'title', 'urlserver')
        weechat.buffer_set(urlserver['buffer'], 'localvar_set_no_log', '1')
        weechat.buffer_set(urlserver['buffer'], 'time_for_each_line', '0')
        weechat.buffer_set(urlserver['buffer'], 'print_hooks_enabled', '0')
        weechat.buffer_clear(urlserver['buffer'])
        keys = sorted(urlserver['urls'])
        for key in keys:
            urlserver_display_url_detail(key)
        weechat.buffer_set(urlserver['buffer'], 'display', '1')


def urlserver_cmd_cb(data, buffer, args):
    """The /urlserver command."""
    global urlserver
    if args == 'start':
        urlserver_server_start()
    elif args == 'restart':
        urlserver_server_restart()
    elif args == 'stop':
        urlserver_server_stop()
    elif args == 'status':
        urlserver_server_status()
    elif args == 'clear':
        urlserver['urls'] = {}
        urlserver['number'] = 0
        weechat.prnt('', 'urlserver: list cleared')
    else:
        urlserver_open_buffer()
    return weechat.WEECHAT_RC_OK


def urlserver_update_urllist(buffer_full_name, buffer_short_name, tags, prefix,
                             message, nick=None):
    """Update urls list and return a list of short urls for message."""
    global urlserver, urlserver_settings

    # skip ignored buffers
    if urlserver_settings['msg_ignore_buffers'] and \
            buffer_full_name in (urlserver_settings['msg_ignore_buffers']
                                 .split(',')):
        return None

    listtags = []
    if tags:
        listtags = tags.split(',')

        # skip ignored tags
        if urlserver_settings['msg_ignore_tags']:
            for itag in urlserver_settings['msg_ignore_tags'].split(','):
                for tag in listtags:
                    if tag.startswith(itag):
                        return None

        # exit if a required tag is missing
        if urlserver_settings['msg_require_tags']:
            for rtag in urlserver_settings['msg_require_tags'].split(','):
                tagfound = False
                for tag in listtags:
                    if tag.startswith(rtag):
                        tagfound = True
                        break
                if not tagfound:
                    return None

    # ignore message is matching the "msg_ignore_regex"
    if urlserver_settings['msg_ignore_regex']:
        if re.search(urlserver_settings['msg_ignore_regex'],
                     prefix + '\t' + message):
            return None

    # extract nick from tags
    if not nick:
        nick = ''
        for tag in listtags:
            if tag.startswith('nick_'):
                nick = tag[5:]
                break

    # get URL min length
    min_length = 0
    try:
        min_length = int(urlserver_settings['url_min_length'])
        # Detect the minimum length based on shorten url length
        if min_length == -1:
            min_length = len(urlserver_short_url(urlserver['number'])) + 1
    except:
        min_length = 0

    # shorten URL(s) in message
    urls_short = []
    for url in urlserver['regex'].findall(message):
        if len(url) >= min_length:
            if urlserver_settings['msg_ignore_dup_urls'] == 'on':
                same_urls = [key for key, value in urlserver['urls'].items()
                             if value[3] == url]
                if same_urls:
                    continue
            number = urlserver['number']
            # don't save urls already shortened
            if not url.startswith(urlserver_get_base_url()):
                urlserver['urls'][number] = (
                    datetime.datetime.now().strftime(
                        urlserver_settings['http_time_format']),
                    nick,
                    buffer_short_name,
                    url,
                    '%s\t%s' % (prefix, message))
                urls_short.append(urlserver_short_url(number))
                if urlserver['buffer']:
                    urlserver_display_url_detail(number)
                urlserver['number'] += 1

    # remove old URLs if we have reach max list size
    urls_amount = 50
    try:
        urls_amount = int(urlserver_settings['urls_amount'])
        if urls_amount <= 0:
            urls_amount = 50
    except:
        urls_amount = 50
    while len(urlserver['urls']) > urls_amount:
        keys = sorted(urlserver['urls'])
        del urlserver['urls'][keys[0]]

    return urls_short


def urlserver_print_cb(data, buffer, time, tags, displayed, highlight, prefix,
                       message):
    """
    Callback for message printed in buffer: display short URLs after message.
    """
    global urlserver, urlserver_settings

    if urlserver_settings['display_urls'] == 'on':
        buffer_full_name = '%s.%s' % (
            weechat.buffer_get_string(buffer, 'plugin'),
            weechat.buffer_get_string(buffer, 'name'))
        if urlserver_settings['buffer_short_name'] == 'on':
            buffer_short_name = weechat.buffer_get_string(buffer, 'short_name')
        else:
            buffer_short_name = buffer_full_name
        urls_short = urlserver_update_urllist(buffer_full_name,
                                              buffer_short_name,
                                              tags,
                                              prefix,
                                              message)
        if urls_short:
            if urlserver_settings['separators'] and \
                    len(urlserver_settings['separators']) == 3:
                separator = ' %s ' % (urlserver_settings['separators'][1])
                urls_string = separator.join(urls_short)
                urls_string = '%s %s %s' % (
                    urlserver_settings['separators'][0],
                    urls_string,
                    urlserver_settings['separators'][2])
            else:
                urls_string = ' | '.join(urls_short)
                urls_string = '[ ' + urls_string + ' ]'
            weechat.prnt_date_tags(
                buffer, 0, 'no_log,notify_none',
                '%s%s' % (weechat.color(urlserver_settings['color']),
                          urls_string))

    return weechat.WEECHAT_RC_OK


def urlserver_modifier_irc_cb(data, modifier, modifier_data, string):
    """Modifier for IRC message: add short URLs at the end of IRC message."""
    global urlserver, urlserver_settings

    if urlserver_settings['display_urls_in_msg'] != 'on':
        return string

    msg = weechat.info_get_hashtable(
        'irc_message_parse',
        {'message': string, 'server': modifier_data})
    if 'nick' not in msg or 'channel' not in msg or 'arguments' not in msg:
        return string

    try:
        message = msg['arguments'].split(' ', 1)[1]
        if message.startswith(':'):
            message = message[1:]
    except:
        return string

    info_name = '%s,%s' % (modifier_data, msg['channel'])
    if weechat.info_get('irc_is_channel', info_name) == '1':
        name = msg['channel']
    else:
        name = msg['nick']
    buffer_full_name = 'irc.%s.%s' % (modifier_data, name)
    if urlserver_settings['buffer_short_name'] == 'on':
        buffer_short_name = name
    else:
        buffer_short_name = buffer_full_name
    urls_short = urlserver_update_urllist(buffer_full_name,
                                          buffer_short_name,
                                          None,
                                          msg['nick'],
                                          message,
                                          msg['nick'])
    if urls_short:
        if urlserver_settings['separators'] and \
                len(urlserver_settings['separators']) == 3:
            separator = ' %s ' % (urlserver_settings['separators'][1])
            urls_string = separator.join(urls_short)
            urls_string = '%s %s %s' % (
                urlserver_settings['separators'][0],
                urls_string,
                urlserver_settings['separators'][2])
        else:
            urls_string = ' | '.join(urls_short)
            urls_string = '[ ' + urls_string + ' ]'

        if urlserver_settings['color_in_msg']:
            urls_string = '\x03%s%s' % (urlserver_settings['color_in_msg'],
                                        urls_string)
        string = "%s %s" % (string, urls_string)

    return string


def urlserver_config_cb(data, option, value):
    """Called when a script option is changed."""
    global urlserver_settings
    pos = option.rfind('.')
    if pos > 0:
        name = option[pos+1:]
        if name in urlserver_settings:
            if name == 'http_allowed_ips':
                urlserver_settings[name] = re.compile(value)
            else:
                urlserver_settings[name] = value
                if name in ('http_hostname', 'http_port'):
                    # don't restart if autostart is disabled and server isn't
                    # already running
                    if urlserver_settings['http_autostart'] == 'on' or \
                            urlserver['socket']:
                        urlserver_server_restart()
    return weechat.WEECHAT_RC_OK


def urlserver_filename():
    """Return name of file used to store list of urls."""
    return os.path.join(weechat.info_get('weechat_dir', ''),
                        'urlserver_list.txt')


def urlserver_read_urls():
    """Read file with URLs."""
    global urlserver
    filename = urlserver_filename()
    if os.path.isfile(filename):
        urlserver['number'] = 0
        try:
            urlserver['urls'] = ast.literal_eval(open(filename, 'r').read())
            keys = sorted(urlserver['urls'])
            if keys:
                urlserver['number'] = keys[-1] + 1
            else:
                urlserver['number'] = 0
        except:
            weechat.prnt('', '%surlserver: error reading file "%s"' % (
                weechat.prefix('error'),
                filename))


def urlserver_write_urls():
    """Write file with URLs."""
    global urlserver
    keys = sorted(urlserver['urls'])
    content = '{\n%s\n}\n' % '\n'.join([
        '  %d: %s,' % (key, str(urlserver['urls'][key])) for key in keys
    ])
    open(urlserver_filename(), 'w').write(content)


def urlserver_end():
    """Script unloaded (oh no, why?)"""
    urlserver_server_stop()
    urlserver_write_urls()
    return weechat.WEECHAT_RC_OK

if __name__ == '__main__' and import_ok:
    if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION,
                        SCRIPT_LICENSE, SCRIPT_DESC, 'urlserver_end', ''):
        # set default settings
        version = weechat.info_get('version_number', '') or 0
        for option, value in urlserver_settings_default.items():
            if weechat.config_is_set_plugin(option):
                urlserver_settings[option] = weechat.config_get_plugin(option)
            else:
                weechat.config_set_plugin(option, value[0])
                urlserver_settings[option] = value[0]
            if int(version) >= 0x00030500:
                weechat.config_set_desc_plugin(
                    option,
                    '%s (default: "%s")' % (value[1], value[0]))

        # detect config changes
        weechat.hook_config('plugins.var.python.%s.*' % SCRIPT_NAME,
                            'urlserver_config_cb', '')

        # add command
        weechat.hook_command(
            SCRIPT_COMMAND,
            SCRIPT_DESC,
            'start|restart|stop|status || clear',
            '  start: start server\n'
            'restart: restart server\n'
            '   stop: stop server\n'
            ' status: display status of server\n'
            '  clear: remove all URLs from list\n\n'
            'Without argument, this command opens new buffer with list of '
            'URLs.\n\n'
            'Initial setup:\n'
            '  - by default, script will listen on a random free port, '
            'you can force a port with:\n'
            '      /set plugins.var.python.urlserver.http_port "1234"\n'
            '  - you can force an IP or custom hostname with:\n'
            '      /set plugins.var.python.urlserver.http_hostname '
            '"111.22.33.44"\n'
            '  - it is strongly recommended to restrict IPs allowed and/or '
            'use auth, for example:\n'
            '      /set plugins.var.python.urlserver.http_allowed_ips '
            '"^(123.45.67.89|192.160.*)$"\n'
            '      /set plugins.var.python.urlserver.http_auth '
            '"user:password"\n'
            '  - if you do not like the default HTML formatting, you can '
            'override the CSS:\n'
            '      /set plugins.var.python.urlserver.http_css_url '
            '"http://example.com/sample.css"\n'
            '      See https://raw.github.com/FiXato/weechat_scripts/master/'
            'urlserver/sample.css\n'
            '  - don\'t like the built-in HTTP server to start automatically? '
            'Disable it:\n'
            '      /set plugins.var.python.urlserver.http_autostart "off"\n'
            '  - have external port 80 or 443 (https) forwarded to your '
            'internal server port? Remove :port with:\n'
            '      /set plugins.var.python.urlserver.http_port_display "80" '
            'or "443" respectively\n'
            '\n'
            'Tip: use URL without key at the end to display list of all URLs '
            'in your browser.',
            'start|restart|stop|status|clear', 'urlserver_cmd_cb', '')

        if urlserver_settings['http_autostart'] == 'on':
            # start mini HTTP server
            urlserver_server_start()

        # load urls from file
        urlserver_read_urls()

        # catch URLs in buffers
        weechat.hook_print('', '', '://', 1, 'urlserver_print_cb', '')

        # modify URLS in irc messages (for relay)
        weechat.hook_modifier('irc_in2_privmsg',
                              'urlserver_modifier_irc_cb', '')
        weechat.hook_modifier('irc_in2_notice',
                              'urlserver_modifier_irc_cb', '')

        # search buffer
        urlserver['buffer'] = weechat.buffer_search('python', SCRIPT_BUFFER)
