defmodule ElixirTalk.Connect do
  use GenServer

  def start_link(args) do
    :gen_server.start_link(__MODULE__, args, [])
  end

  def quit(pid) do
    :gen_server.cast(pid, :stop)
  end

  def call(pid, oper_with_data, timeout \\ 5_000) do
    :gen_server.call(pid, oper_with_data, timeout)
  end

  def init([host, port, timeout]) do
    case :gen_tcp.connect(host, port, [:binary, {:packet, 0}, {:active, false}], timeout) do
      {:ok, socket} ->
        {:ok, [socket: socket]}
      {:error, reason} ->
        {:stop, reason}
    end
  end

  def handle_call({:put, data, opts}, _from, [socket: socket] = state) do
    # put <pri> <delay> <ttr> <bytes>\r\n<data>\r\n
    #TODO check the opts limit
    pri   = Keyword.get(opts, :pri, 0)
    delay = Keyword.get(opts, :delay, 0)
    ttr   = Keyword.get(opts, :ttr, 60)
    bytes = byte_size(data)
    bin_data = "put #{pri} #{delay} #{ttr} #{bytes}\r\n#{data}\r\n"
    :gen_tcp.send(socket, bin_data)
    {:ok, result} = :gen_tcp.recv(socket, 0)
    {:reply, get_result(result), state}
  end

  def handle_call({:release, id, opts}, _from, [socket: socket] = state) do
    pri   = Keyword.get(opts, :pri, 0)
    delay = Keyword.get(opts, :delay, 0)
    bin_data = "release #{id} #{pri} #{delay}\r\n"
    :gen_tcp.send(socket, bin_data)
    {:ok, result} = :gen_tcp.recv(socket, 0)
    {:reply, get_result(result), state}
  end

  def handle_call({cmd, data, opt}, _from, [socket: socket] = state) do
    bin_data = String.replace("#{cmd}", "_", "-") <> " #{data} #{opt}\r\n"
    :gen_tcp.send(socket, bin_data)
    {:ok, result} = :gen_tcp.recv(socket, 0)
    {:reply, get_result(result), state}
  end

  def handle_call({cmd, data}, _from, [socket: socket] = state) do
    cmd = String.replace("#{cmd}", "_", "-")
    bin_data = cond do
      data == [] -> "#{cmd}\r\n"
      true       -> "#{cmd} #{data}\r\n"
    end

    :gen_tcp.send(socket, bin_data)
    {:ok, result} = :gen_tcp.recv(socket, 0)
    {:reply, get_result(result), state}
  end

  def handle_call(cmd, _from, [socket: socket] = state) do
    cmd = Atom.to_string(cmd) |> String.replace("_", "-")
    :gen_tcp.send(socket, "#{cmd}\r\n")
    {:ok, result} = :gen_tcp.recv(socket, 0)
    {:reply, get_result(result), state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def terminate(:normal, [socket: socket]) do
    :gen_tcp.close(socket)
    :ok
  end

  ######################
  ## Privacy Apis
  ######################

  defp get_result(result) do
    case String.contains?(result, " ") do
      true ->
        [h, tail] = String.split(result, " ", parts: 2, trim: true)

        # kick out the terminated '\r\n'
        str = str_slice(tail)
        case Regex.match?(~r/^\d{1,}$/, str) do
          true  -> do_result(h, String.to_integer(str))
          false -> do_result(h, str)
        end
      false ->
        do_result(str_slice(result))
    end
  end

  defp do_result(result) do
    str_to_atom(result)
  end
  defp do_result("USING", data) do
    {str_to_atom("USING"), data}
  end
  defp do_result(res, num) when is_integer(num) do
    {str_to_atom(res), num}
  end
  defp do_result("OK", tail) do
    if String.contains?(tail, ":") do
      do_stats(tail)
    else
      do_tubes(tail)
    end
  end
  defp do_result(res = "RESERVED", tail) do
    # handle RESERVE
    # <id> <bytes>\r\n<data>
    [id, byte, data] = String.split(tail, ~r/ |\r\n/, parts: 3)
    {str_to_atom(res), String.to_integer(id), {String.to_integer(byte), data}}
  end

  defp str_to_atom(str) do
    String.downcase(str) |> String.to_atom
  end

  # If str isn't a number, return itself
  defp str_to_num(str) do
    cond do
      Regex.match?(~r/^\d{1,}$/, str) ->
        String.to_integer(str)
      Regex.match?(~r/^\d{1,}\.\d{1,}$/, str) ->
        String.to_float(str)
      true ->
        str
    end
  end

  # When in Elixir 1.0.3, String.slice/2 works on graphemes and
  # '\r\n' is considered a single grapheme.
  # see more here https://github.com/elixir-lang/elixir/issues/3224
  defp str_slice(str) do
    :erlang.binary_part(str, 0, byte_size(str) - byte_size("\r\n"))
  end

  defp do_tubes(tail) do
    tubes = String.split(tail, "\n")
    Enum.filter_map(tubes, &Regex.match?(~r/^-\ /, &1), &String.slice(&1, 2..-1))
  end

  defp do_stats(tail) do
    origin = String.split(tail, "\n")
    str_to_keyword = fn(str) ->
      [a, b] = String.split(str, ": ", parts: 2)
      {str_to_atom(a), str_to_num(b)}
    end
    Enum.filter_map(origin, &(String.contains?(&1, ":")), str_to_keyword)
  end
end
