module TestScript
  def ping
    "pong"
  end
end

Ernie.expose(:test_script, TestScript)
