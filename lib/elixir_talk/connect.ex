defmodule ElixirTalk.Connect do
  use GenServer
  require Logger

  import ElixirTalk.Protocol

  defmodule State do
    defstruct host: '127.0.0.1',
              port: 11300,
              conn: nil,
              from: nil,
              connect_timeout: 5_000,
              recv_timeout: 5_000,
              reconnect: false,
              reconnect_sleep: 0
  end

  @sock_opts [mode: :binary, packet: 0, active: false, reuseaddr: true]

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, [])
  end

  def quit(pid) do
    GenServer.cast(pid, :stop)
  end

  def call(pid, oper_with_data, timeout \\ 5_000) do
    GenServer.call(pid, oper_with_data, timeout)
  end

  def init(opts) do
    Process.flag(:trap_exit, true)

    host = Keyword.get(opts, :host)
    port = Keyword.get(opts, :port)

    recv_timeout    = Keyword.get(opts, :recv_timeout, 5_000)
    connect_timeout = Keyword.get(opts, :connect_timeout, 5_000)

    state = %State{host: host,
                   port: port,
                   recv_timeout: recv_timeout,
                   connect_timeout: connect_timeout}
    case connect(state) do
      {:ok, conn}      -> {:ok, %{state | conn: conn}}
      {:error, reason} -> {:stop, {:shutdown, reason}}
    end
  end

  def handle_call({:put, data, opts}, _from, state) do
    # put <pri> <delay> <ttr> <bytes>\r\n<data>\r\n
    #TODO check the opts limit
    pri   = Keyword.get(opts, :pri, 0)
    delay = Keyword.get(opts, :delay, 0)
    ttr   = Keyword.get(opts, :ttr, 60)
    bytes = byte_size(data)
    bin_data = "put #{pri} #{delay} #{ttr} #{bytes}\r\n#{data}\r\n"
    send_msg(bin_data, state)
  end

  def handle_call({:release, id, opts}, _from, state) do
    pri   = Keyword.get(opts, :pri, 0)
    delay = Keyword.get(opts, :delay, 0)
    bin_data = "release #{id} #{pri} #{delay}\r\n"
    send_msg(bin_data, state)
  end

  def handle_call({cmd, data, opt}, _from, state) do
    bin_data = String.replace("#{cmd}", "_", "-") <> " #{data} #{opt}\r\n"
    send_msg(bin_data, state)
  end

  def handle_call({cmd, data}, _from, state) do
    cmd = String.replace("#{cmd}", "_", "-")
    bin_data = cond do
      data == [] -> "#{cmd}\r\n"
      true       -> "#{cmd} #{data}\r\n"
    end
    send_msg(bin_data, state)
  end

  def handle_call(cmd, _from, state) do
    cmd = Atom.to_string(cmd) |> String.replace("_", "-")
    bin_data = cmd <> "\r\n"
    send_msg(bin_data, state)
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def terminate(_, %State{conn: conn}) do
    :gen_tcp.close(conn)
    :ok
  end

  ######################
  ## Privacy Apis
  ######################

  defp connect(%State{host: host, port: port, connect_timeout: timeout}) do
    Logger.info "connect with #{host}: #{port}"
    :gen_tcp.connect(host, port, @sock_opts, timeout)
  end

  defp send_msg(_msg, %State{conn: nil, reconnect: false}=state) do
    {:reply, {:error, :noconnection}, state}
  end
  defp send_msg(msg, %State{conn: nil}=state) do
    {:ok, conn} = connect(state)
    send_msg(msg, %{state | conn: conn})
  end
  defp send_msg(msg, %State{conn: conn, recv_timeout: timeout}=state) do
    case :gen_tcp.send(conn, msg) do
      :ok ->
        case recv_msg(conn, <<>>, timeout) do
          {:ok, result} -> {:reply, result, state}
          {:error, err} -> {:reply, {:error, err}, state}
        end

      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  defp recv_msg(conn, buffer, timeout) do
    case :gen_tcp.recv(conn, 0, timeout) do
      {:ok, data} ->
        packet = <<buffer :: binary, data :: binary>>
        case parse(packet) do
          :more -> recv_msg(conn, packet, timeout)
          {:ok, result, _rest} -> {:ok, result}
        end
      error ->
        error
    end
  end

end
