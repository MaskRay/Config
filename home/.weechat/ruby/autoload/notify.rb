require 'ruby-growl'

def weechat_init
  Weechat.register('notify', 'Ray Song', '0.0', 'GPL', 'notify', '', '')
  Weechat.hook_print '', 'irc_privmsg', '', 1, 'notify', ''
  Weechat::WEECHAT_RC_OK
end

def notify data, buffer, date, tags, displayed, highlight, prefix, message
  g = Growl.new 'localhost', 'ruby-growl'
  g.add_notification 'weechat'

  if Weechat.buffer_get_string(buffer, 'localvar_type') == 'private' or highlight == '1'
    g.notify 'weechat', prefix, message
  end

  Weechat::WEECHAT_RC_OK
end
