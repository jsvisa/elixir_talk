defmodule ElixirTalk.Connect do
  use GenServer.Behaviour

  def start_link(args) do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, args, [])
  end

end
