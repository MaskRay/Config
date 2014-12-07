#require 'ruby-growl'

TIME = 5000

def weechat_init
  Weechat.register 'notify', 'Ray Song', '0.0', 'GPL', 'notify', '', ''
  Weechat.hook_signal "weechat_pv", "notify", ""
  Weechat.hook_signal "weechat_highlight", "notify", ""
  Weechat::WEECHAT_RC_OK
end

$last = {}

def notify data, signal, message
  #g = Growl.new 'localhost', 'ruby-growl'
  #g.add_notification 'weechat'
  channel, message = message.split "\t", 2
  #g.notify 'weechat', channel, message
  t = Time.now.to_i
  if t - $last.fetch(channel, 0) > 10
    IO.popen(['notify-send', '-i', '/home/ray/Icons/weechat.png', '-a', channel, '-t', TIME.to_s, '--', message]).close
  end
  $last[channel] = t
  Weechat::WEECHAT_RC_OK
end
