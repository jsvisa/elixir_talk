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
    IO.puts "#{oper} #{inspect data} #{inspect opts}"
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
    IO.puts bin_data
    :gen_tcp.send(socket, bin_data)
    {:ok, result} = :gen_tcp.recv(socket, 0)
    {:reply, get_result(result), state}
  end

  def handle_call({:release, data, opts}, _from, state = State[socket: socket]) do
    pri = Keyword.get(opts, :pri, 0)
    delay = Keyword.get(opts, :delay, 0)
    bin_data = "release #{pri} #{delay}\r\n"
    IO.puts bin_data
    :gen_tcp.send(socket, bin_data)
    {:ok, result} = :gen_tcp.recv(socket, 0)
    IO.puts "#{inspect data}"
    {:reply, get_result(result), state}
  end

  def handle_call({cmd, data, opt}, _from, state = State[socket: socket]) do
    bin_data = String.replace("#{cmd}", "_", "-") <> " #{data} #{opt}\r\n"
    IO.puts bin_data
    :gen_tcp.send(socket, bin_data)
    {:ok, result} = :gen_tcp.recv(socket, 0)
    {:reply, get_result(result), state}
  end

  def handle_call({cmd, data}, _from, state = State[socket: socket]) do
    # IO.puts inspect cmd
    cmd = String.replace("#{cmd}", "_", "-")
    bin_data = cond do
      data == [] -> "#{cmd}\r\n"
      true -> "#{cmd} #{data}\r\n"
    end
    # IO.puts bin_data

    :gen_tcp.send(socket, bin_data)
    {:ok, result} = :gen_tcp.recv(socket, 0)
    {:reply, get_result(result), state}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def terminate(:normal, state) do
    State[socket: socket] = state
    :gen_tcp.close(socket)
  end

  defp get_result(result) do
    # IO.puts inspect result
    [h | t] = String.split(result, " ", global: false)
    t = list_to_bitstring(t)
    # kick out the '\r\n
    str = String.replace(t, "\r\n", "")
    cond do
      Regex.match?(%r/^\d{1,}$/, str) -> do_result(h, binary_to_integer(str))
      true -> do_result(h, t)
    end
  end

  defp do_result(res, "") do
    str_to_atom String.replace(res, "\r\n", "")
  end
  defp do_result("USING", data) do
    {str_to_atom("USING"), String.replace(data, "\r\n", "")}
  end
  defp do_result(res, num) when is_integer(num) do
    {str_to_atom(res), num}
  end
  defp do_result("OK", tail) do
    # IO.inspect tail
    if String.contains?(tail, ":") do
      do_stats(tail)
    else
      do_tubes(tail)
    end
  end
  defp do_result(res, tail) do
    # <id> <bytes>\r\n<data>\r\n
    [id, t] = String.split(tail, " ", global: false)
    [_count, data] = String.split(t, "\r\n", global: false)
    #TODO befor jose merge my code to Elixir master branch, it doesn't work
    # data = String.slice(data, 0..-2)
    data = String.slice(data, 0, String.length(data) - 2)
    {str_to_atom(res), binary_to_integer(id), data}
  end

  defp str_to_atom(str) do
    {:ok, str_list} = String.to_char_list(String.downcase(str))
    list_to_atom(str_list)
  end

  # when str isn't a number, return itself
  defp str_to_num(str) do
    cond do
      Regex.match?(%r/^\d{1,}$/, str) ->
        binary_to_integer(str)
      Regex.match?(%r/^\d{1,}\.\d{1,}/, str) ->
        binary_to_float(str)
      true ->
        str
    end
  end

  defp do_tubes(tail) do
    tubes = String.split(tail, "\n")
    Enum.filter_map(tubes, fn(x) -> Regex.match?(%r/^-\ /, x) end, fn(x) -> String.slice(x, 2..-1) end)
  end

  defp do_stats(tail) do
    origin = String.split(tail, "\n")
    str_to_keyword = fn(str) ->
      [a, b] = String.split(str, ": ")
      {str_to_atom(a), str_to_num(b)}
    end
    Enum.filter_map(origin, fn(x) -> String.contains?(x, ":") end, str_to_keyword)
  end


end
