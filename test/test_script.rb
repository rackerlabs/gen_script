module TestScript
  def ping
    "pong"
  end
end

GenScript.expose(:test_script, TestScript)
