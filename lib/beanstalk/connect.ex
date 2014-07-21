defmodule Beanstalk.Connect do
  use GenServer

  def start_link(args) do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, args, [])
  end

  def quit() do
    :gen_server.cast(__MODULE__, :stop)
  end

  def call(oper, data \\ []) do
    :gen_server.call(__MODULE__, {oper, data})
  end

  def call(oper, data, opts) do
    :gen_server.call(__MODULE__, {oper, data, opts})
  end

  def call_forever(oper, data \\ []) do
    :gen_server.call(__MODULE__, {oper, data}, :infinity)
  end

  def init([host, port, timeout]) do
    {:ok, host} = :inet.parse_address(String.to_char_list host)
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
    bytes = String.length(data)
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
      true -> "#{cmd} #{data}\r\n"
    end

    :gen_tcp.send(socket, bin_data)
    {:ok, result} = :gen_tcp.recv(socket, 0)
    {:reply, get_result(result), state}
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
    [h | t] = String.split(result, " ", global: false)
    t = List.to_string(t)
    # kick out the '\r\n
    str = String.replace(t, "\r\n", "")
    cond do
      Regex.match?(~r/^\d{1,}$/, str) -> do_result(h, String.to_integer(str))
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
    if String.contains?(tail, ":") do
      do_stats(tail)
    else
      do_tubes(tail)
    end
  end
  defp do_result(res, tail) do
    # <id> <bytes>\r\n<data>\r\n
    [id, data] = String.split(tail, "\r\n", parts: 3, trim: true)
    {str_to_atom(res), String.to_integer(id), data}
  end

  defp str_to_atom(str) do
    String.downcase(str) |> String.to_atom
  end

  # when str isn't a number, return itself
  defp str_to_num(str) do
    cond do
      Regex.match?(~r/^\d{1,}$/, str) ->
        String.to_integer(str)
      Regex.match?(~r/^\d{1,}\.\d{1,}/, str) ->
        String.to_float(str)
      true ->
        str
    end
  end

  defp do_tubes(tail) do
    tubes = String.split(tail, "\n")
    Enum.filter_map(tubes, &Regex.match?(~r/^-\ /, &1), &String.slice(&1, 2..-1))
  end

  defp do_stats(tail) do
    origin = String.split(tail, "\n")
    str_to_keyword = fn(str) ->
      [a, b] = String.split(str, ":", parts: 2)
      {str_to_atom(a), str_to_num(b)}
    end
    Enum.filter_map(origin, &String.contains?(&1, ":"), str_to_keyword)
  end
end
