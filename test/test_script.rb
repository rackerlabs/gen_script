module TestScript
  def ping
    "pong"
  end

  def error
    t[:error, :testing]
  end

  def timeout
    sleep(1)
  end

  def exception
    raise "Testing"
  end

  def die
    exit!(-1)
  end
end

GenScript.expose(:test_script, TestScript)
