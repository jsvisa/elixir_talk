defmodule ElixirTalk do

  def connect(host \\ "127.0.0.1", port \\ 11300, timeout \\ :infinity) do
    ElixirTalk.Connect.start_link([host, port, timeout])
  end

  def quit do
    ElixirTalk.Connect.quit
  end

  def put(data, opts \\ []) do
    ElixirTalk.Connect.call(:put, data, opts)
  end

  def use(tube) do
    ElixirTalk.Connect.call(:use, tube)
  end

  def watch(tube) do
    ElixirTalk.Connect.call(:watch, tube)
  end

  def ignore(tube) do
    ElixirTalk.Connect.call(:ignore, tube)
  end

  def delete(id) do
    ElixirTalk.Connect.call(:delete, id)
  end

  def touch(id) do
    ElixirTalk.Connect.call(:touch, id)
  end

  def peek(id) do
    ElixirTalk.Connect.call(:peek, id)
  end

  def peek_ready do
    ElixirTalk.Connect.call(:peek_ready)
  end

  def peek_delayed do
    ElixirTalk.Connect.call(:peek_delayed)
  end

  def peek_buried do
    ElixirTalk.Connect.call(:peek_buried)
  end

  def kick(bound) do
    ElixirTalk.Connect.call(:kick, bound)
  end

  def kick_job(id) do
    ElixirTalk.Connect.call(:kick_job, id)
  end

  def stats_job(id) do
    ElixirTalk.Connect.call(:stats_job, id)
  end

  def stats_tube(tube) do
    ElixirTalk.Connect.call(:stats_tube, tube)
  end

  def stats do
    ElixirTalk.Connect.call(:stats)
  end

  def list_tubes do
    ElixirTalk.Connect.call(:list_tubes)
  end

  def list_tube_used do
    ElixirTalk.Connect.call(:list_tube_used)
  end

  def list_tubes_watched do
    ElixirTalk.Connect.call(:list_tubes_watched)
  end

  def reserve do
    ElixirTalk.Connect.call_forever(:reserve)
  end

  def reserve(timeout) do
    ElixirTalk.Connect.call_forever(:reserve_with_timeout, timeout)
  end

  def bury(id, pri \\ 0) do
    ElixirTalk.Connect.call(:bury, id, pri)
  end

  def pause_tube(tube, delay) do
    ElixirTalk.Connect.call(:pause_tube, tube, delay)
  end

  def release(id, opts \\ []) do
    ElixirTalk.Connect.call(:release, id, opts)
  end

end
