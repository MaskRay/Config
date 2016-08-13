# -*- coding: utf-8 -*-
#
# Copyright (C) 2011 Sebastien Helleu <flashcode@flashtux.org>
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
# WeeChat theme manager.
# (this script requires WeeChat 0.3.5 or newer)
#
# History:
#
# 2011-09-28, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.1: dev snapshot
# 2011-02-22, Sebastien Helleu <flashcode@flashtux.org>:
#     start dev
#

SCRIPT_NAME    = 'theme'
SCRIPT_AUTHOR  = 'Sebastien Helleu <flashcode@flashtux.org>'
SCRIPT_VERSION = '0.1-dev'
SCRIPT_LICENSE = 'GPL3'
SCRIPT_DESC    = 'WeeChat theme manager'

SCRIPT_COMMAND = 'theme'

import_weechat_ok = True
import_other_ok   = True

try:
    import weechat
except ImportError:
    import_weechat_ok = False

try:
    import sys
    import os
    import re
    import datetime
    import time
except ImportError as e:
    print('Missing package(s) for %s: %s' % (SCRIPT_NAME, e))
    import_other_ok = False

THEME_CONFIG_FILE_NAME = "theme"

COLOR_ATTRIBUTES = ('*', '_', '!')

# config file and options
theme_config_file   = ""
theme_config_option = {}

theme_bars    = 'input|nicklist|status|title'
theme_plugins = 'weechat|alias|aspell|charset|fifo|irc|logger|relay|rmodifier|xfer'

theme_options_include_re = (
    r'^weechat\.bar\.(%s)\.color.*' % theme_bars,
    r'^weechat\.look\.buffer_time_format$',
    r'^(%s)\.color\..*' % theme_plugins,
    r'^(%s)\.look\..*color.*' % theme_plugins,
)

theme_options_exclude_re = (
    r'^weechat.look.color_pairs_auto_reset$',
    r'^weechat.look.color_real_white$',
    r'^weechat.look.color_basic_force_bold$',
    r'^irc\.look\.',
)

# =================================[ config ]=================================

def theme_config_init():
    """Initialization of configuration file. Sections: ???."""
    global theme_config_file, theme_config_option
    theme_config_file = weechat.config_new(THEME_CONFIG_FILE_NAME,
                                           'theme_config_reload_cb', '')
    if not theme_config_file:
        return
    
    # section "color"
    section_color = weechat.config_new_section(
        theme_config_file, 'color', 0, 0, '', '', '', '', '', '', '', '', '', '')
    if not section_color:
        weechat.config_free(theme_config_file)
        return
    theme_config_option['color_script'] = weechat.config_new_option(
        theme_config_file, section_color,
        'script', 'color', 'Color for script names', '', 0, 0,
        'cyan', 'cyan', 0, '', '', '', '', '', '')
    theme_config_option['color_installed'] = weechat.config_new_option(
        theme_config_file, section_color,
        'installed', 'color', 'Color for "installed" indicator', '', 0, 0,
        'yellow', 'yellow', 0, '', '', '', '', '', '')
    theme_config_option['color_running'] = weechat.config_new_option(
        theme_config_file, section_color,
        'running', 'color', 'Color for "running" indicator', '', 0, 0,
        'lightgreen', 'lightgreen', 0, '', '', '', '', '', '')
    theme_config_option['color_obsolete'] = weechat.config_new_option(
        theme_config_file, section_color,
        'obsolete', 'color', 'Color for "obsolete" indicator', '', 0, 0,
        'lightmagenta', 'lightmagenta', 0, '', '', '', '', '', '')
    theme_config_option['color_unknown'] = weechat.config_new_option(
        theme_config_file, section_color,
        'unknown', 'color', 'Color for "unknown status" indicator', '', 0, 0,
        'lightred', 'lightred', 0, '', '', '', '', '', '')
    theme_config_option['color_language'] = weechat.config_new_option(
        theme_config_file, section_color,
        'language', 'color', 'Color for language names', '', 0, 0,
        'lightblue', 'lightblue', 0, '', '', '', '', '', '')
    
    # section "themes"
    section_themes = weechat.config_new_section(
        theme_config_file, 'themes', 0, 0, '', '', '', '', '', '', '', '', '', '')
    if not section_themes:
        weechat.config_free(theme_config_file)
        return
    theme_config_option['themes_url'] = weechat.config_new_option(
        theme_config_file, section_themes,
        'url', 'string', 'URL for file with list of themes', '', 0, 0,
        'http://www.weechat.org/files/themes.json.gz',
        'http://www.weechat.org/files/themes.json.gz', 0, '', '', '', '', '', '')
    theme_config_option['themes_cache_expire'] = weechat.config_new_option(
        theme_config_file, section_themes,
        'cache_expire', 'integer', 'Local cache expiration time, in minutes '
        '(-1 = never expires, 0 = always expires)', '',
        -1, 60*24*365, '60', '60', 0, '', '', '', '', '', '')
    theme_config_option['themes_dir'] = weechat.config_new_option(
        theme_config_file, section_themes,
        'dir', 'string', 'Local directory for themes', '', 0, 0,
        '%h/themes', '%h/themes', 0, '', '', '', '', '', '')

def theme_config_reload_cb(data, config_file):
    """Reload configuration file."""
    return weechat.config_read(config_file)

def theme_config_read():
    """Read configuration file."""
    global theme_config_file
    return weechat.config_read(theme_config_file)

def theme_config_write():
    """Write configuration file."""
    global theme_config_file
    return weechat.config_write(theme_config_file)

def theme_config_color(color):
    """Get a color from configuration."""
    global theme_config_option
    option = theme_config_option.get('color_%s' % color, '')
    if not option:
        return ''
    return weechat.color(weechat.config_string(option))

def theme_config_get_dir():
    """Return themes directory, with expanded WeeChat home dir."""
    global theme_config_option
    return weechat.config_string(
        theme_config_option['themes_dir']).replace('%h',
                                                   weechat.info_get('weechat_dir', ''))

def theme_config_get_backup():
    """Return name of backup theme (by default "~/.weechat/themes/_backup.theme")."""
    return '%s/_backup.theme' % theme_config_get_dir()

def theme_config_get_undo():
    """Return name of undo file (by default "~/.weechat/themes/_undo.theme")."""
    return '%s/_undo.theme' % theme_config_get_dir()

def theme_config_create_dir():
    """Create "themes" directory."""
    directory = theme_config_get_dir()
    if not os.path.isdir(directory):
        os.makedirs(directory, mode=0700)

def theme_config_get_cache_filename():
    """Get local cache filename, based on URL."""
    global theme_config_option
    return '%s/%s' % (theme_config_get_dir(),
                      os.path.basename(weechat.config_string(theme_config_option['scripts_url'])))

# =================================[ themes ]=================================

class Theme:

    def __init__(self, filename=None):
        self.filename = filename
        self.props = {}
        self.listprops = []
        self.options = {}
        self.theme_ok = True
        if self.filename:
            self.theme_ok = self.load(self.filename)
        else:
            self.init_weechat()
        self.nick_prefixes = self._get_nick_prefixes()

    def isok(self):
        return self.theme_ok

    def _option_is_used(self, option):
        global theme_options_include_re, theme_options_exclude_re
        for regex in theme_options_exclude_re:
            if re.search(regex, option):
                return False
        for regex in theme_options_include_re:
            if re.search(regex, option):
                return True
        return False

    def _get_nick_prefixes(self):
        """Get dict with nick prefixes."""
        prefixes = {}
        for prefix in self.options.get('irc.color.nick_prefixes', '').split(';'):
            values = prefix.split(':', 1)
            if len(values) == 2:
                prefixes[values[0]] = values[1]
        return prefixes

    def _get_attr_color(self, color):
        """Return tuple with attributes and color."""
        m = re.match('([*_!]*)(.*)', color)
        if m:
            return m.group(1), m.group(2)
        return '', color

    def _get_color_without_alias(self, color):
        """Return color without alias (color can be "fg", "fg,bg" or "fg:bg")."""
        pos = color.find(',')
        if pos < 0:
            pos = color.find(':')
        if pos > 0:
            fg = color[0:pos]
            bg = color[pos + 1:]
        else:
            fg = color
            bg = ''
        attr, col = self._get_attr_color(fg)
        fg = '%s%s' % (attr, self.palette.get(col, col))
        attr, col = self._get_attr_color(bg)
        bg = '%s%s' % (attr, self.palette.get(col, col))
        if bg:
            return '%s%s%s' % (fg, color[pos:pos + 1], bg)
        return fg

    def _replace_color_alias(self, match):
        value = match.group()[2:-1]
        if value in self.palette:
            value = self.palette[value]
        return '${%s}' % value

    def init_weechat(self):
        """Initialize theme using current WeeChat options (aliases are replaced with their values from palette)."""
        # get palette options
        self.palette = {}
        infolist = weechat.infolist_get('option', '', 'weechat.palette.*')
        while weechat.infolist_next(infolist):
            option_name = weechat.infolist_string(infolist, 'option_name')
            value = weechat.infolist_string(infolist, 'value')
            self.palette[value] = option_name
        weechat.infolist_free(infolist)
        # get color options (replace aliases by values from palette)
        self.options = {}
        infolist = weechat.infolist_get('option', '', '')
        while weechat.infolist_next(infolist):
            full_name = weechat.infolist_string(infolist, 'full_name')
            if self._option_is_used(full_name):
                value = weechat.infolist_string(infolist, 'value')
                self.options[full_name] = self._get_color_without_alias(value)
        weechat.infolist_free(infolist)
        # replace aliases in chat_nick_colors
        option = 'weechat.color.chat_nick_colors'
        colors = []
        for color in self.options.get(option, '').split(','):
            colors.append(self._get_color_without_alias(color))
        if colors:
            self.options[option] = ','.join(colors)
        # replace aliases in buffer_time_format
        option = 'weechat.look.buffer_time_format'
        if option in self.options:
            value = re.compile(r'\$\{[^\}]+\}').sub(self._replace_color_alias, self.options[option])
            if value:
                self.options[option] = value
        # build dict with nick prefixes (and replace alisases)
        prefixes = []
        option = 'irc.color.nick_prefixes'
        for prefix in self.options.get(option, '').split(';'):
            values = prefix.split(':', 1)
            if len(values) == 2:
                prefixes.append('%s:%s' % (values[0], self._get_color_without_alias(values[1])))
        if prefixes:
            self.options[option] = ';'.join(prefixes)
        # delete palette
        del self.palette

    def prnt(self, message):
        try:
            weechat.prnt('', message)
        except:
            print(message)

    def prnt_error(self, message):
        try:
            weechat.prnt('', '%s%s' % (weechat.prefix('error'), message))
        except:
            print(message)

    def load(self, filename):
        self.options = {}
        try:
            lines = open(filename, 'rb').readlines()
            for line in lines:
                line = str(line.strip().decode('utf-8'))
                if line.startswith('#'):
                    m = re.match('^# \\$([A-Za-z]+): (.*)', line)
                    if m:
                        self.props[m.group(1)] = m.group(2)
                        self.listprops.append(m.group(1))
                else:
                    items = line.split('=', 1)
                    if len(items) == 2:
                        value = items[1].strip()
                        if value.startswith('"') and value.endswith('"'):
                            value = value[1:-1]
                        self.options[items[0].strip()] = value
            return True
        except:
            self.prnt('Error loading theme "%s"' % filename)
            return False

    def save(self, filename):
        names = self.options.keys()
        names.sort()
        try:
            f = open(filename, 'w')
            version = weechat.info_get('version', '')
            pos = version.find('-')
            if pos > 0:
                version = version[0:pos]
            header = ('#',
                      '# -- WeeChat theme --',
                      '# $name: %s' % os.path.basename(filename),
                      '# $date: %s' % datetime.date.today(),
                      '# $weechat: %s' % version,
                      '# $script: %s.py %s' % (SCRIPT_NAME, SCRIPT_VERSION),
                      '#\n')
            f.write('\n'.join(header))
            for option in names:
                f.write('%s = "%s"\n' % (option, self.options[option]))
            f.close()
            self.prnt('Theme saved to "%s"' % filename)
        except:
            self.prnt_error('Error writing theme to "%s"' % filename)
            raise

    def show(self, header):
        """Display content of theme."""
        names = self.options.keys()
        names.sort()
        self.prnt('')
        self.prnt(header)
        for name in names:
            self.prnt('  %s %s= %s%s' % (name, weechat.color('chat_delimiters'),
                                         weechat.color('chat_value'), self.options[name]))

    def info(self, header):
        """Display info about theme."""
        self.prnt('')
        self.prnt(header)
        for prop in self.listprops:
            self.prnt('  %s: %s%s' % (prop, weechat.color('chat_value'), self.props[prop]))
        numerrors = 0
        for name in self.options:
            if not weechat.config_get(name):
                numerrors += 1
        if numerrors == 0:
            text = 'all OK'
        else:
            text = 'WARNING: %d option(s) not found in your WeeChat' % numerrors
        self.prnt('  options: %s%d%s (%s)' % (weechat.color('chat_value'), len(self.options), weechat.color('reset'), text))

    def install(self):
        try:
            numset = 0
            numerrors = 0
            for name in self.options:
                option = weechat.config_get(name)
                if option:
                    if weechat.config_option_set(option, self.options[name], 1) == weechat.WEECHAT_CONFIG_OPTION_SET_ERROR:
                        self.prnt_error('Error setting option "%s" to value "%s" (running an old WeeChat?)' % (name, self.options[name]))
                        numerrors += 1
                    else:
                        numset += 1
                else:
                    self.prnt('Warning: option not found: "%s" (running an old WeeChat?)' % name)
                    numerrors += 1
            errors = ''
            if numerrors > 0:
                errors = ', %d error(s)' % numerrors
            if self.filename:
                self.prnt('Theme "%s" installed (%d options set%s)' % (self.filename, numset, errors))
            else:
                self.prnt('Theme installed (%d options set%s)' % (numset, errors))
        except:
            if self.filename:
                self.prnt_error('Failed to install theme "%s"' % self.filename)
            else:
                self.prnt_error('Failed to install theme')

    def nick_prefix_color(self, prefix):
        """Get color for a nick prefix."""
        modes = 'qaohv'
        prefixes = '~&@%+'
        pos = prefixes.find(prefix)
        if pos < 0:
            return ''
        while pos < len(modes):
            if modes[pos] in self.nick_prefixes:
                return self.nick_prefixes[modes[pos]]
            pos += 1
        return self.nick_prefixes.get('*', '')

# =============================[ themes / html ]==============================

class HtmlTheme(Theme):

    def __init__(self, filename=None, chat_width=85, chat_height=25, prefix_width=10, nicklist_width=10):
        Theme.__init__(self, filename)
        self.chat_width = chat_width
        self.chat_height = chat_height
        self.prefix_width = prefix_width
        self.nicklist_width = nicklist_width

    def html_color(self, index):
        """Return HTML color with index in table of 256 colors."""
        terminal_colors = \
            '000000cd000000cd00cdcd000000cdcd00cd00cdcde5e5e54d4d4dff000000ff00ffff000000ffff00ff00ffffffffff' \
            '00000000002a0000550000800000aa0000d4002a00002a2a002a55002a80002aaa002ad400550000552a005555005580' \
            '0055aa0055d400800000802a0080550080800080aa0080d400aa0000aa2a00aa5500aa8000aaaa00aad400d40000d42a' \
            '00d45500d48000d4aa00d4d42a00002a002a2a00552a00802a00aa2a00d42a2a002a2a2a2a2a552a2a802a2aaa2a2ad4' \
            '2a55002a552a2a55552a55802a55aa2a55d42a80002a802a2a80552a80802a80aa2a80d42aaa002aaa2a2aaa552aaa80' \
            '2aaaaa2aaad42ad4002ad42a2ad4552ad4802ad4aa2ad4d455000055002a5500555500805500aa5500d4552a00552a2a' \
            '552a55552a80552aaa552ad455550055552a5555555555805555aa5555d455800055802a5580555580805580aa5580d4' \
            '55aa0055aa2a55aa5555aa8055aaaa55aad455d40055d42a55d45555d48055d4aa55d4d480000080002a800055800080' \
            '8000aa8000d4802a00802a2a802a55802a80802aaa802ad480550080552a8055558055808055aa8055d480800080802a' \
            '8080558080808080aa8080d480aa0080aa2a80aa5580aa8080aaaa80aad480d40080d42a80d45580d48080d4aa80d4d4' \
            'aa0000aa002aaa0055aa0080aa00aaaa00d4aa2a00aa2a2aaa2a55aa2a80aa2aaaaa2ad4aa5500aa552aaa5555aa5580' \
            'aa55aaaa55d4aa8000aa802aaa8055aa8080aa80aaaa80d4aaaa00aaaa2aaaaa55aaaa80aaaaaaaaaad4aad400aad42a' \
            'aad455aad480aad4aaaad4d4d40000d4002ad40055d40080d400aad400d4d42a00d42a2ad42a55d42a80d42aaad42ad4' \
            'd45500d4552ad45555d45580d455aad455d4d48000d4802ad48055d48080d480aad480d4d4aa00d4aa2ad4aa55d4aa80' \
            'd4aaaad4aad4d4d400d4d42ad4d455d4d480d4d4aad4d4d40808081212121c1c1c2626263030303a3a3a4444444e4e4e' \
            '5858586262626c6c6c7676768080808a8a8a9494949e9e9ea8a8a8b2b2b2bcbcbcc6c6c6d0d0d0dadadae4e4e4eeeeee'
        color = terminal_colors[index*6:(index*6)+6]
        #if color in ('000000', 'e5e5e5'): # keep black or 'default' (gray)
        #    return color
        r = int(color[0:2], 16)
        g = int(color[2:4], 16)
        b = int(color[4:6], 16)
        r = min(r * (1.5 - (r / 510.0)), 255)
        g = min(g * (1.5 - (r / 510.0)), 255)
        b = min(b * (1.5 - (r / 510.0)), 255)
        return '%02x%02x%02x' % (r, g, b)

    def html_style(self, fg, bg):
        """Return HTML style with WeeChat fg and bg colors."""
        weechat_basic_colors = {
            'default': 7, 'black': 0, 'darkgray': 8, 'red': 1, 'lightred': 9,
            'green': 2, 'lightgreen': 10, 'brown': 3, 'yellow': 11, 'blue': 4,
            'lightblue': 12, 'magenta': 5, 'lightmagenta': 13, 'cyan': 6,
            'lightcyan': 14, 'gray': 7, 'white': 15 }
        delim = max(fg.find(','), fg.find(':'))
        if delim > 0:
            bg = fg[delim + 1:]
            fg = fg[0:delim]
        bold = ''
        underline = ''
        reverse = False
        while fg[0] in COLOR_ATTRIBUTES:
            if fg[0] == '*':
                bold = '; font-weight: bold'
            elif fg[0] == '_':
                underline = '; text-decoration: underline'
            elif fg[0] == '!':
                reverse = True
            fg = fg[1:]
        while bg[0] in COLOR_ATTRIBUTES:
            bg = bg[1:]
        if fg == 'default':
            fg = self.options['fg']
        if bg == 'default':
            bg = self.options['bg']
        if bold and fg in ('black', '0'):
            fg = 'darkgray'
            reverse = ''
        if reverse:
            fg2 = bg
            bg = fg
            fg = fg2
        if fg == 'white' and self.whitebg:
            fg = 'black'
        num_fg = 0
        num_bg = 0
        if fg in weechat_basic_colors:
            num_fg = weechat_basic_colors[fg]
        else:
            try:
                num_fg = int(fg)
            except:
                self.prnt('Warning: unknown fg color "%s", using "default" instead' % fg)
                num_fg = weechat_basic_colors['default']
        if bg in weechat_basic_colors:
            num_bg = weechat_basic_colors[bg]
        else:
            try:
                num_bg = int(bg)
            except:
                self.prnt('Warning: unknown bg color "%s", using "default" instead' % bg)
                num_bg = weechat_basic_colors['default']
        style = 'color: #%s; background-color: #%s%s%s' % (self.html_color(num_fg),
                                                           self.html_color(num_bg),
                                                           bold, underline)
        return style

    def html_string(self, string, maxlen, optfg='fg', optbg='bg'):
        """Write html string using fg/bg colors."""
        fg = optfg
        bg = optbg
        if fg in self.options:
            fg = self.options[optfg]
        if bg in self.options:
            bg = self.options[optbg]
        if maxlen >= 0:
            string = string.ljust(maxlen)
        else:
            string = string.rjust(maxlen * -1)
        return '<span style="%s">%s</span>' % (self.html_style (fg, bg), string)

    def html_nick(self, nicks, index, prefix, usecolor, highlight, maxlen, optfg='fg', optbg='bg'):
        """Print a nick."""
        nick = nicks[index]
        nickfg = optfg
        if usecolor and optfg != 'weechat.color.nicklist_away':
            nick_colors = self.options['weechat.color.chat_nick_colors'].split(',')
            nickfg = nick_colors[index % len(nick_colors)]
        if usecolor and nick == self.html_nick_self:
            nickfg = 'weechat.color.chat_nick_self'
        if nick[0] in ('@', '%', '+'):
            color = self.nick_prefix_color(nick[0]) or optfg
            str_prefix = self.html_string(nick[0], 1, color, optbg)
            nick = nick[1:]
        else:
            str_prefix = self.html_string(' ', 1, optfg, optbg)
        length = 1 + len(nick)
        if not prefix:
            str_prefix = ''
            maxlen += 1
            length -= 1
        padding = ''
        if length < abs(maxlen):
            padding = self.html_string('', abs(maxlen) - length, optfg, optbg)
        if highlight:
            nickfg = 'weechat.color.chat_highlight'
            optbg = 'weechat.color.chat_highlight_bg'
        string = str_prefix + self.html_string(nick, 0, nickfg, optbg)
        if maxlen < 0:
            return padding + string
        return string + padding

    def html_concat(self, messages, width, optfg, optbg):
        """Concatenate some messages with colors."""
        string = ''
        remaining = width
        for msg in messages:
            if msg[0] != '':
                string += self.html_string(msg[1], 0, msg[0], optbg)
                remaining -= len(msg[1])
            else:
                string += self.html_nick((msg[1],), 0, False, True, False, 0, optfg, optbg)
                remaining -= len(msg[1])
                if msg[1][0] in ('@', '%', '+'):
                    remaining += 1
        string += self.html_string('', remaining, optfg, optbg)
        return string

    def _html_apply_colors(self, match):
        string = match.group()
        end = string.find('}')
        if end < 0:
            return string
        color = string[2:end]
        text = string[end + 1:]
        return self.html_string(text, 0, color)

    def _html_apply_color_chat_time_delimiters(self, match):
        return self.html_string(match.group(), 0, 'weechat.color.chat_time_delimiters')

    def html_chat_time(self, msgtime):
        """Return formatted time with colors."""
        option = 'weechat.look.buffer_time_format'
        if self.options[option].find('${') >= 0:
            str_without_colors = re.sub(r'\$\{[^\}]+\}', '', self.options[option])
            length = len(time.strftime(str_without_colors, msgtime))
            value = re.compile(r'\$\{[^\}]+\}[^\$]*').sub(self._html_apply_colors, self.options[option])
        else:
            value = time.strftime(self.options[option], msgtime)
            length = len(value)
            value = re.compile(r'[^0-9]+').sub(self._html_apply_color_chat_time_delimiters, value)
            value = self.html_string(value, 0, 'weechat.color.chat_time')
        return (time.strftime(value, msgtime), length)

    def html_chat(self, hhmmss, prefix, messages):
        """Print a message in chat area."""
        delimiter = self.html_string(':', 0, 'weechat.color.chat_time_delimiters', 'weechat.color.chat_bg')
        str_datetime = '2010-12-25 %02d:%02d:%02d' % (hhmmss[0], hhmmss[1], hhmmss[2])
        t = time.strptime(str_datetime, '%Y-%m-%d %H:%M:%S')
        (str_time, length_time) = self.html_chat_time(t)
        return str_time + prefix + \
            self.html_string(' &#9474; ', 0, 'weechat.color.chat_prefix_suffix', 'weechat.color.chat_bg') + \
            self.html_concat(messages, self.chat_width - length_time - self.prefix_width - 3,
                             'weechat.color.chat', 'weechat.color.chat_bg')

    def to_html(self):
        """Print HTML version of theme."""
        self.html_nick_self = 'mario'
        channel = '#weechat'
        oldtopic = 'Welcome'
        newtopic = 'Welcome to %s - help channel for WeeChat' % channel
        nicks = ('@carl', '@jessika', '@louise', '%Melody', '%Diego', '+Max',
                 'sheryl', 'Harold^', 'richard', 'celia', 'Eva', 'freddy', 'lee',
                 'madeleine', self.html_nick_self, 'mila', 'peter', 'tina', 'Vince', 'warren', 'warren2')
        nicks_hosts = ('test@foo.com', 'something@host.com')
        chat_msgs = ('Hello!',
                     'hi mario, I just tested your patch',
                     'I would like to ask something',
                     'just ask!',
                     'WeeChat is great?',
                     'yes',
                     'indeed',
                     'sure',
                     'of course!',
                     'affirmative',
                     'all right',
                     'obviously...',
                     'certainly!')
        html = []
        #html.append('<pre style="line-height: 1.2em">')
        html.append('<pre>')
        width = self.chat_width + 1 + self.nicklist_width

        # title bar
        html.append(self.html_string(newtopic, width,
                                     'weechat.bar.title.color_fg', 'weechat.bar.title.color_bg'))

        # chat
        chat = []
        str_prefix_join = self.html_string('-->', self.prefix_width * -1, 'weechat.color.chat_prefix_join', 'weechat.color.chat_bg')
        str_prefix_quit = self.html_string('<--', self.prefix_width * -1, 'weechat.color.chat_prefix_quit', 'weechat.color.chat_bg')
        str_prefix_network = self.html_string('--', self.prefix_width * -1, 'weechat.color.chat_prefix_network', 'weechat.color.chat_bg')
        str_prefix_empty = self.html_string('', self.prefix_width * -1, 'weechat.color.chat', 'weechat.color.chat_bg')
        chat.append(self.html_chat((9, 10, 00),
                                   str_prefix_join,
                                   (('', self.html_nick_self),
                                    ('weechat.color.chat_delimiters', ' ('),
                                    ('weechat.color.chat_host',       nicks_hosts[0]),
                                    ('weechat.color.chat_delimiters', ')'),
                                    ('irc.color.message_join',        ' has joined '),
                                    ('weechat.color.chat_channel',    channel))))
        chat.append(self.html_chat((9, 10, 25),
                                   self.html_nick(nicks, 8, True, True, False, self.prefix_width * -1),
                                   (('weechat.color.chat', chat_msgs[0]),)))
        chat.append(self.html_chat((9, 11, 2),
                                   str_prefix_network,
                                   (('', nicks[0]),
                                    ('weechat.color.chat',         ' has changed topic for '),
                                    ('weechat.color.chat_channel', channel),
                                    ('weechat.color.chat',         ' from "'),
                                    ('irc.color.topic_old',        oldtopic),
                                    ('weechat.color.chat',         '"'))))
        chat.append(self.html_chat((9, 11, 2),
                                   str_prefix_empty,
                                   (('weechat.color.chat',  'to "'),
                                    ('irc.color.topic_new', newtopic),
                                    ('weechat.color.chat',  '"'))))
        chat.append(self.html_chat((9, 11, 36),
                                   self.html_nick(nicks, 16, True, True, True, self.prefix_width * -1),
                                   (('weechat.color.chat', chat_msgs[1]),)))
        chat.append(self.html_chat((9, 12, 4),
                                   str_prefix_quit,
                                   (('', 'joe'),
                                    ('weechat.color.chat_delimiters', ' ('),
                                    ('weechat.color.chat_host',       nicks_hosts[1]),
                                    ('weechat.color.chat_delimiters', ')'),
                                    ('irc.color.message_quit',        ' has left '),
                                    ('weechat.color.chat_channel',    channel),
                                    ('weechat.color.chat_delimiters', ' ('),
                                    ('irc.color.reason_quit',         'bye!'),
                                    ('weechat.color.chat_delimiters', ')'))))
        chat.append(self.html_chat((9, 15, 58),
                                   self.html_nick(nicks, 12, True, True, False, self.prefix_width * -1),
                                   (('weechat.color.chat', chat_msgs[2]),)))
        chat.append(self.html_chat((9, 16, 12),
                                   self.html_nick(nicks, 0, True, True, False, self.prefix_width * -1),
                                   (('weechat.color.chat', chat_msgs[3]),)))
        chat.append(self.html_chat((9, 16, 27),
                                   self.html_nick(nicks, 12, True, True, False, self.prefix_width * -1),
                                   (('weechat.color.chat', chat_msgs[4]),)))
        for i in range(5, len(chat_msgs)):
            chat.append(self.html_chat((9, 17, (i - 5) * 4),
                                       self.html_nick(nicks, i - 2, True, True, False, self.prefix_width * -1),
                                       (('weechat.color.chat', chat_msgs[i]),)))
        chat_empty = self.html_string(' ', self.chat_width, 'weechat.color.chat', 'weechat.color.chat_bg')

        # separator (between chat and nicklist)
        str_separator = self.html_string('&#9474;', 0, 'weechat.color.separator', 'weechat.color.chat_bg')

        # nicklist
        nicklist = []
        for index in range(0, len(nicks)):
            fg = 'weechat.bar.nicklist.color_fg'
            if nicks[index].endswith('a'):
                fg = 'weechat.color.nicklist_away'
            nicklist.append(self.html_nick(nicks, index, True, True, False, self.nicklist_width, fg, 'weechat.bar.nicklist.color_bg'))
        nicklist_empty = self.html_string('', self.nicklist_width, 'weechat.bar.nicklist.color_fg', 'weechat.bar.nicklist.color_bg')

        # print chat + nicklist
        for i in range (0, self.chat_height):
            if i < len(chat):
                str1 = chat[i]
            else:
                str1 = chat_empty
            if i < len(nicklist):
                str2 = nicklist[i]
            else:
                str2 = nicklist_empty
            html.append('%s%s%s' % (str1, str_separator, str2))

        # status
        html.append(self.html_concat((('weechat.bar.status.color_delim',      '['),
                                      ('weechat.color.status_time',           '12:34'),
                                      ('weechat.bar.status.color_delim',      '] ['),
                                      ('weechat.bar.status.color_fg',         '18'),
                                      ('weechat.bar.status.color_delim',      '] ['),
                                      ('weechat.bar.status.color_fg',         'irc'),
                                      ('weechat.bar.status.color_delim',      '/'),
                                      ('weechat.bar.status.color_fg',         'freenode'),
                                      ('weechat.bar.status.color_delim',      '] '),
                                      ('weechat.color.status_number',         '2'),
                                      ('weechat.bar.status.color_delim',      ':'),
                                      ('weechat.color.status_name',           '#weechat'),
                                      ('weechat.bar.status.color_delim',      '('),
                                      ('irc.color.item_channel_modes',        '+nt'),
                                      ('weechat.bar.status.color_delim',      '){'),
                                      ('weechat.bar.status.color_fg',         '%d' % len(nicks)),
                                      ('weechat.bar.status.color_delim',      '} ['),
                                      ('weechat.bar.status.color_fg',         'Act: '),
                                      ('weechat.color.status_data_highlight', '3'),
                                      ('weechat.bar.status.color_delim',      ':'),
                                      ('weechat.bar.status.color_fg',         '#linux'),
                                      ('weechat.bar.status.color_delim',      ','),
                                      ('weechat.color.status_data_private',   '18'),
                                      ('weechat.bar.status.color_delim',      ','),
                                      ('weechat.color.status_data_msg',       '4'),
                                      ('weechat.bar.status.color_delim',      ','),
                                      ('weechat.color.status_data_other',     '5'),
                                      ('weechat.bar.status.color_delim',      ','),
                                      ('weechat.color.status_data_other',     '6'),
                                      ('weechat.bar.status.color_delim',      ']')),
                                     width, 'weechat.bar.status.color_fg', 'weechat.bar.status.color_bg'))

        # input
        html.append(self.html_concat((('weechat.bar.input.color_delim', '['),
                                      (self.nick_prefix_color('+'),     '+'),
                                      ('irc.color.input_nick',          self.html_nick_self),
                                      ('weechat.bar.input.color_delim', '('),
                                      ('weechat.bar.input.color_fg',    'i'),
                                      ('weechat.bar.input.color_delim', ')] '),
                                      ('weechat.bar.input.color_fg',    'this is misspelled '),
                                      ('aspell.look.color',             'woord'),
                                      ('weechat.bar.input.color_fg',    ' '),
                                      ('cursor',                        ' ')),
                                     width, 'weechat.bar.input.color_fg', 'weechat.bar.input.color_bg'))

        # end
        html.append('</pre>')
        del self.html_nick_self
        return '\n'.join(html)

    def get_html(self, whitebg=False):
        if whitebg:
            self.options['fg'] = 'black'
            self.options['bg'] = 'white'
            self.options['cursor'] = '!black'
        else:
            self.options['fg'] = '250'
            self.options['bg'] = 'black'
            self.options['cursor'] = '!yellow'
        self.whitebg = whitebg
        html = self.to_html()
        del self.whitebg
        del self.options['fg']
        del self.options['bg']
        del self.options['cursor']
        return html

    def save_html(self, filename, whitebg=False):
        html = self.get_html(whitebg)
        try:
            f = open(filename, 'w')
            f.write(html)
            f.close()
            self.prnt('Theme exported as HTML to "%s"' % filename)
        except:
            self.prnt_error('Error writing HTML to "%s"' % filename)
            raise

# ================================[ command ]=================================

def theme_cmd(data, buffer, args):
    """Callback for /theme command."""
    if args == '':
        weechat.command('', '/help %s' % SCRIPT_COMMAND)
        return weechat.WEECHAT_RC_OK
    argv = args.strip().split(' ', 1)
    if len(argv) == 0:
        return weechat.WEECHAT_RC_OK

    if argv[0] in ('list', 'install'):
        weechat.prnt('', '%s: action "%s" not developed' % (SCRIPT_NAME, argv[0]))
        return weechat.WEECHAT_RC_OK

    # check arguments
    if len(argv) < 2:
        if argv[0] in ('install', 'installfile', 'save', 'export'):
            weechat.prnt('', '%s: too few arguments for action "%s"'
                         % (SCRIPT_NAME, argv[0]))
            return weechat.WEECHAT_RC_OK

    # execute asked action
    if argv[0] == 'info':
        filename = None
        if len(argv) >= 2:
            filename = argv[1]
        theme = Theme(filename)
        if filename:
            theme.info('Info about theme "%s":' % filename)
        else:
            theme.info('Info about current theme:')
    elif argv[0] == 'show':
        filename = None
        if len(argv) >= 2:
            filename = argv[1]
        theme = Theme(filename)
        if filename:
            theme.show('Content of theme "%s":' % filename)
        else:
            theme.show('Content of current theme:')
    elif argv[0] == 'installfile':
        theme = Theme()
        theme.save(theme_config_get_undo())
        theme = Theme(argv[1])
        if theme.isok():
            theme.install()
    elif argv[0] == 'undo':
        theme = Theme(theme_config_get_undo())
        if theme.isok():
            theme.install()
    elif argv[0] == 'save':
        theme = Theme()
        theme.save(argv[1])
    elif argv[0] == 'backup':
        theme = Theme()
        theme.save(theme_config_get_backup())
    elif argv[0] == 'restore':
        theme = Theme(theme_config_get_backup())
        if theme.isok():
            theme.install()
    elif argv[0] == 'export':
        htheme = HtmlTheme()
        whitebg = False
        htmlfile = argv[1]
        argv2 = args.strip().split(' ', 2)
        if len(argv2) >= 3 and argv2[1] == 'white':
            whitebg = True
            htmlfile = argv2[2]
        htheme.save_html(htmlfile, whitebg)

    return weechat.WEECHAT_RC_OK

# ==================================[ main ]==================================

def theme_init():
    """Called when script is loaded."""
    theme_config_create_dir()
    filename = theme_config_get_backup()
    if not os.path.isfile(filename):
        theme = Theme()
        theme.save(filename)

def theme_cmdline_usage():
    """Display usage."""
    padding = ' ' * len(sys.argv[0])
    print('')
    print('Usage:  %s  --export <themefile> <htmlfile> [white]' % sys.argv[0])
    print('        %s  --info <filename>' % padding)
    print('        %s  --help' % padding)
    print('')
    print('  -e, --export  export a theme file to HTML')
    print('  -i, --info    display info about a theme')
    print('  -h, --help    display this help')
    print('')
    sys.exit(0)

def theme_cmdline():
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help'):
        theme_cmdline_usage()
    elif len(sys.argv) > 1:
        if sys.argv[1] in ('-e', '--export'):
            if len(sys.argv) < 4:
                theme_cmdline_usage()
            whitebg = 'white' in sys.argv[4:]
            htheme = HtmlTheme(sys.argv[2])
            htheme.save_html(sys.argv[3], whitebg)
        elif sys.argv[1] in ('-i', '--info'):
            if len(sys.argv) < 3:
                theme_cmdline_usage()
            theme = Theme(sys.argv[2])
            theme.info('Info about theme "%s":' % sys.argv[2])
        else:
            theme_cmdline_usage()

if __name__ == '__main__' and import_other_ok:
    if import_weechat_ok:
        if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
                            SCRIPT_DESC, '', ''):
            theme_config_init()
            theme_config_read()
            theme_init()
            weechat.hook_command(SCRIPT_COMMAND,
                                 'WeeChat theme manager',
                                 'list [<text>] || info|show [<theme>] || install <theme>'
                                 ' || installfile <file> || undo || save <file> || backup || restore'
                                 ' || export [-white] <file>',
                                 '       list: list themes (search text if given)\n'
                                 '       info: show info about theme (without argument: for current theme)\n'
                                 '       show: show all options in theme (without argument: for current theme)\n'
                                 '    install: install a theme from repository\n'
                                 'installfile: load theme from a file\n'
                                 '       undo: undo last theme install\n'
                                 '       save: save current theme in a file\n'
                                 '     backup: backup current theme (by default in ~/.weechat/themes/_backup.theme); this is done the first time script is loaded\n'
                                 '    restore: restore theme backuped by script\n'
                                 '     export: save current theme as HTML in a file (with "-white": use white background in HTML)\n\n'
                                 'Examples:\n'
                                 '  /' + SCRIPT_COMMAND + ' save /tmp/flashcode.theme => save current theme',
                                 'info %(filename)'
                                 ' || show %(filename)'
                                 ' || install %(themes)'
                                 ' || installfile %(filename)'
                                 ' || undo'
                                 ' || save %(filename)'
                                 ' || backup'
                                 ' || restore'
                                 ' || export -white|%(filename) %(filename)',
                                 'theme_cmd', '')
    else:
        theme_cmdline()
