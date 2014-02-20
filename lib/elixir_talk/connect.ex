defmodule ElixirTalk.Connect do
  use GenServer.Behaviour

  defrecord State, socket: -1

  def start_link(args) do
    IO.puts "start_link"
    IO.puts "#{inspect args}"
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, args, [])
  end

  def quit() do
    :gen_server.cast(__MODULE__, :stop)
  end

  def call(oper, data \\ []) do
    :gen_server.call(__MODULE__, {oper, data})
  end

  def call(oper, data, opts) do
    IO.puts "put #{inspect data}"
    :gen_server.call(__MODULE__, {oper, data, opts})
  end

  def call_forever(oper, data \\ []) do
    :gen_server.call(__MODULE__, {oper, data}, :infinity)
  end

  def init([host, port, timeout]) do
    IO.puts "init"
    host = String.split(host, ".") |> Enum.map(fn(x) -> binary_to_integer(x) end) |> list_to_tuple
    case :gen_tcp.connect(host, port, [:binary, {:packet, 0}, {:active, false}], timeout) do
      {:ok, socket} ->
        {:ok, State.new(socket: socket)}
      {:error, reason} ->
        {:stop, reason}
    end
  end

  def handle_call({:put, data, opts}, _from, state = State[socket: socket]) do
    # put <pri> <delay> <ttr> <bytes>\r\n<data>\r\n
    #TODO check the opts limit
    pri = Keyword.get(opts, :pri, 0)
    delay = Keyword.get(opts, :delay, 0)
    ttr = Keyword.get(opts, :ttr, 60)
    bytes = String.length(data)
    bin_data = "put #{pri} #{delay} #{ttr} #{bytes}\r\n#{data}\r\n"
    :gen_tcp.send(socket, bin_data)
    # TODO handle the recv data
    result = :gen_tcp.recv(socket, 0)
    IO.puts "#{inspect data}"
    {:reply, result, state}
  end

  def handle_call({cmd, data}, _from, state = State[socket: socket]) do
    bin_data = cond do
      data == [] -> "#{cmd}\r\n"
      true -> "#{cmd} #{data}\r\n"
    end |> String.replace("_", "-")

    :gen_tcp.send(socket, bin_data)
    # TODO handle the recv data
    result = :gen_tcp.recv(socket, 0)
    IO.puts "#{inspect data}"
    {:reply, result, state}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def terminate(:normal, state) do
    State[socket: socket] = state
    :gen_tcp.close(socket)
  end

end
