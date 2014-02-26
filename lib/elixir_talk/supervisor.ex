defmodule ElixirTalk.Supervisor do
  use Supervisor.Behaviour

  def start_link(args) do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, args)
  end

  def init(args) do
    # The child process is restarted only if it terminates abnormally,
    # i.e. with another exit reason than :normal, :shutdown or { :shutdown, term };}
    children = [ worker(ElixirTalk.Connect, [args], restart: :transient) ]
    supervise(children, strategy: :one_for_one)
  end
end
