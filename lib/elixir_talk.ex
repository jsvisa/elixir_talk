defmodule ElixirTalk do
  alias ElixirTalk.Connect

  @moduledoc """
  ElixirTalk - A beanstalkd client coding with Elixir

  from
  Copyright 2014-2016 by jsvisa(delweng@gmail.com)
  """
  @type result :: {:inserted, non_neg_integer} | {:buried, non_neg_integer} | {:expected_crlf} | :job_to_big | :darining
  @vsn 1.0

  @doc """
  Connect to the beanstalkd server.
  """

  @spec connect(:inet.ip_address | :inet.hostname, integer, integer) :: {:ok, pid} | {:error, term}
  def connect(host \\ '127.0.0.1', port \\ 11300, timeout \\ :infinity) do
    Connect.start_link([host, port, timeout])
  end

  @doc """
  Close the connection to server.
  """
  @spec quit(pid) :: :ok
  def quit(pid) do
    Connect.quit(pid)
  end

  @doc """
  Put a job to the current tube.

  The opts can be any combination of
  `:pri`    : an integer < 2**32. Jobs with smaller priority values will be
              scheduled before jobs with larger priorities. The most urgent priority is 0;
              the least urgent priority is 4,294,967,295.
  `:delay`  : an integer number of seconds to wait before putting the job in
              the ready queue. The job will be in the "delayed" state during this time.
  `:ttr`    : time to run -- is an integer number of seconds to allow a worker
              to run this job. This time is counted from the moment a worker reserves
              this job. If the worker does not delete, release, or bury the job within
              `:ttr` seconds, the job will time out and the server will release the job.
              The minimum ttr is 1. If the client sends 0, the server will silently
              increase the ttr to 1.
  """
  @spec put(pid, bitstring) :: result
  @spec put(pid, bitstring, Keyword) :: result
  def put(pid, data, opts \\ []) do
    Connect.call(pid, {:put, data, opts})
  end

  @doc """
  Use a tube to `put` jobs.
  """

  @spec use(pid, bitstring) :: {:using, bitstring}
  def use(pid, tube) do
    Connect.call(pid, {:use, tube})
  end

  @doc """
  Add the named tube to the watch list for the current connection.
  A reserve command will take a job from any of the tubes in the
  watch list.
  """

  @spec watch(pid, bitstring) :: {:watcing, non_neg_integer}
  def watch(pid, tube) do
    Connect.call(pid, {:watch, tube})
  end

  @doc """
  Remove the named tube from the watch list for the current connection.
  """
  @spec ignore(pid, bitstring) :: {:watching, non_neg_integer} | :not_ingored
  def ignore(pid, tube) do
    Connect.call(pid, {:ignore, tube})
  end

  @doc """
  Remove a job from the server entirely. It is normally used
  by the client when the job has successfully run to completion. A client can
  delete jobs that it has reserved, ready jobs, delayed jobs, and jobs that are
  buried.
  """

  @spec delete(pid, non_neg_integer) :: :deleted | :not_found
  def delete(pid, id) do
    Connect.call(pid, {:delete, id})
  end

  @doc """
  Allow a worker to request more time to work on a job.
  This is useful for jobs that potentially take a long time, but you still want
  the benefits of a TTR pulling a job away from an unresponsive worker.  A worker
  may periodically tell the server that it's still alive and processing a job
  (e.g. it may do this on DEADLINE_SOON). The command postpones the auto
  release of a reserved job until TTR seconds from when the command is issued.
  """

  @spec touch(pid, non_neg_integer) :: :touched | :not_found
  def touch(pid, id) do
    Connect.call(pid, {:touch, id})
  end

  @doc """
  Let the client inspect a job in the system. Peeking the given job id
  """

  @spec peek(pid, non_neg_integer) :: {:found, non_neg_integer} | :not_found
  def peek(pid, id) do
    Connect.call(pid, {:peek, id})
  end

  @doc """
  Peeking the next ready job.
  """

  @spec peek_ready(pid) :: {:found, non_neg_integer} | :not_found
  def peek_ready(pid) do
    Connect.call(pid, :peek_ready)
  end

  @doc """
  Peeking the delayed job with the shortest delay left.
  """

  @spec peek_delayed(pid) :: {:found, non_neg_integer} | :not_found
  def peek_delayed(pid) do
    Connect.call(pid, :peek_delayed)
  end

  @doc """
  Peeking the next job in the list of buried jobs.
  """

  @spec peek_buried(pid) :: {:found, non_neg_integer} | :not_found
  def peek_buried(pid) do
    Connect.call(pid, :peek_buried)
  end

  @doc """
  Move jobs into the ready queue. If there are any buried jobs, it will only kick buried jobs.
  Otherwise it will kick delayed jobs.

  Apply only to the currently used tube.
  """

  @spec kick(pid, non_neg_integer) :: {:kicked, non_neg_integer}
  def kick(pid, bound) do
    Connect.call(pid, {:kick, bound})
  end

  @doc """
  Similar to `kick(bound)`, if the given job id exists and is in a buried or
  delayed state, it will be moved to the ready queue of the the same tube where it
  currently belongs.
  """

  @spec kick_job(pid, non_neg_integer) :: :kicked | :not_found
  def kick_job(pid, id) do
    Connect.call(pid, {:kick_job, id})
  end

  @doc """
  Give statistical information about the system as a whole.
  """

  @spec stats(pid) :: Keyword
  def stats(pid) do
    Connect.call(pid, :stats)
  end

  @doc """
  Similar to `stats/0`, gives statistical information about the specified job if
  it exists.
  """

  @spec stats_job(pid, non_neg_integer) :: Keyword | :not_found
  def stats_job(pid, id) do
    Connect.call(pid, {:stats_job, id})
  end

  @doc """
  Similar to `stats/0`, gives statistical information about the specified tube
  if it exists.
  """

  @spec stats_tube(pid, bitstring) :: Keyword | :not_found
  def stats_tube(pid, tube) do
    Connect.call(pid, {:stats_tube, tube})
  end

  @doc """
  Return a list of all existing tubes in the server.
  """

  @spec list_tubes(pid) :: List
  def list_tubes(pid) do
    Connect.call(pid, :list_tubes)
  end

  @doc """
  Return the tube currently being used by the client.
  """

  @spec list_tube_used(pid) :: {:using, binary}
  def list_tube_used(pid) do
    Connect.call(pid, :list_tube_used)
  end

  @doc """
  Return the tubes currently being watched by the client.
  """

  @spec list_tubes_watched(pid) :: List
  def list_tubes_watched(pid) do
    Connect.call(pid, :list_tubes_watched)
  end

  @doc """
  Get a job from the currently watched tubes.
  """

  @spec reserve(pid) :: {:reserved, non_neg_integer, bitstring}
  def reserve(pid) do
    Connect.call(pid, :reserve, :infinity)
  end

  @doc """
  Get a job from the currently watched tubes with timeout of seconds.
  """

  @spec reserve(pid, non_neg_integer) :: {:reserved, non_neg_integer, {non_neg_integer, binary}} |
                                         :deadline_soon |
                                         :timed_out
  def reserve(pid, timeout) do
    Connect.call(pid, {:reserve_with_timeout, timeout}, :infinity)
  end

  @doc """
  Put a job into the "buried" state. Buried jobs are put into a
  FIFO linked list and will not be touched by the server again until a client
  kicks them with the `kick` command.
  """

  @spec bury(pid, non_neg_integer) :: :buried | :not_found
  @spec bury(pid, non_neg_integer, non_neg_integer) :: :buried | :not_found
  def bury(pid, id, pri \\ 0) do
    Connect.call(pid, {:bury, id, pri})
  end

  @doc """
  Delay any new job being reserved for a given time.
  """

  @spec pause_tube(pid, bitstring, non_neg_integer) :: :paused | :not_found
  def pause_tube(pid, tube, delay) do
    Connect.call(pid, {:pause_tube, tube, delay})
  end

  @doc """
  Put a reserved job back into the ready queue (and marks its state as "ready")
  to be run by any client. It is normally used when the job fails because of a transitory error.

  The opts can any combination of
  `:pri`    : a new priority to assign to the job;
  `:delay`  : an integer number of seconds to wait before putting the job back in the ready queue.
              The job will be in the "delayed" state during this time.
  """

  @spec release(pid, non_neg_integer) :: :released | :buried | :not_found
  @spec release(pid, non_neg_integer, Keyword) :: :released | :buried | :not_found
  def release(pid, id, opts \\ []) do
    Connect.call(pid, {:release, id, opts})
  end

end
