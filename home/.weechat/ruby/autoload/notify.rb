require 'ruby-growl'

def weechat_init
  Weechat.register 'notify', 'Ray Song', '0.0', 'GPL', 'notify', '', ''
  Weechat.hook_signal "weechat_pv", "notify", "" 
  Weechat.hook_signal "weechat_highlight", "notify", "" 
  Weechat::WEECHAT_RC_OK
end

def notify data, signal, message
  g = Growl.new 'localhost', 'ruby-growl'
  g.add_notification 'weechat'
  channel, message = message.split "\t", 2
  g.notify 'weechat', channel, message
  Weechat::WEECHAT_RC_OK
end
