#!/usr/bin/env ruby
require 'pathname'

cwd = Pathname(__FILE__).dirname
load  cwd + '../priv/ernie.rb'

module TestScript
  def ping
    "pong"
  end
end

Ernie.expose(:test_script, TestScript)
