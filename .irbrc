require 'rubygems' unless defined? Gem
require 'irb/completion'
#require 'irb/ext/save-history'
require 'interactive_editor'
require "bond"
require 'looksee'
require 'wirble'

IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"
IRB.conf[:EVAL_HISTORY] = 200
#
#histfile = "~/.irb_history"
#
IRB.conf[:PROMPT_MODE] = :SIMPLE
#
Wirble.init
Wirble.colorize

# Prompt behavior

# Awesome Print
#IRB::Irb.class_eval do
#  def output_value
#    ap @context.last_value
#  end
#end


#class Object
#  def local_methods
#    (methods - Object.instance_methods).sort
#  end
#end

Bond.start :readline => :ruby

#alias q exit
#
