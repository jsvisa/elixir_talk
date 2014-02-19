defmodule ElixirTalk do
  def connect(ip \\ '127.0.0.1', port \\ 11300, timeout \\ 5000) do
    ElixirTalk.Connect.start_link([ip, port, timeout])
  end

  def close(pid) do
    ElixirTalk.Connect.close(pid)
  end
end
