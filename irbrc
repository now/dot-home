# -*- coding: utf-8 -*-

require 'irb/completion'

if IRB.conf[:PROMPT]
  IRB.conf[:PROMPT][:NOW] = {
    :PROMPT_I => "::: name: %m :: item: %n :::\n",
    :PROMPT_N => nil,
    :PROMPT_S => nil,
    :PROMPT_C => nil,
    :RETURN => "# ⇒ %s\n",
    :AUTO_INDENT => true
  }

  IRB.conf[:PROMPT_MODE] = :NOW
end
