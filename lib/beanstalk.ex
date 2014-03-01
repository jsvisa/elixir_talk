defmodule Beanstalk do

  @moduledoc """
  Beanstalk - A beanstalkd client coding with Elixir

  from
  Copyright 2014 by jsvisa(delweng@gmail.com)
  """
  @type result :: {:inserted, non_neg_integer} | {:buried, non_neg_integer} | {:expected_crlf} | :job_to_big | :darining
  @vsn 0.1

  @doc """
  Connect to the beanstalkd server.
  """

  @spec connect(bitstring, integer, integer) :: {:ok, pid} | {:error, term}
  def connect(host \\ "127.0.0.1", port \\ 11300, timeout \\ :infinity) do
    case Beanstalk.Supervisor.start_link([host, port, timeout]) do
      {:ok, pid} ->
        {:ok, pid}
      {:error, {:already_started, pid}} ->
        # ignore the return value of Connect's start_link
        {_, _} = Beanstalk.Connect.start_link([host, port, timeout])
        {:ok, pid}
      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Close the connection to server.
  """
  @spec quit() :: :ok
  def quit do
    Beanstalk.Connect.quit
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
  @spec put(bitstring) :: result
  @spec put(bitstring, Keyword) :: result
  def put(data, opts \\ []) do
    Beanstalk.Connect.call(:put, data, opts)
  end

  @doc """
  Use a tube to `put` jobs.
  """

  @spec use(bitstring) :: {:using, bitstring}
  def use(tube) do
    Beanstalk.Connect.call(:use, tube)
  end

  @doc """
  Add the named tube to the watch list for the current connection.
  A reserve command will take a job from any of the tubes in the
  watch list.
  """

  @spec watch(bitstring) :: {:watcing, non_neg_integer}
  def watch(tube) do
    Beanstalk.Connect.call(:watch, tube)
  end

  @doc """
  Remove the named tube from the watch list for the current connection.
  """
  @spec ignore(bitstring) :: {:watching, non_neg_integer} | :not_ingored
  def ignore(tube) do
    Beanstalk.Connect.call(:ignore, tube)
  end

  @doc """
  Remove a job from the server entirely. It is normally used
  by the client when the job has successfully run to completion. A client can
  delete jobs that it has reserved, ready jobs, delayed jobs, and jobs that are
  buried.
  """

  @spec delete(non_neg_integer) :: :deleted | :not_found
  def delete(id) do
    Beanstalk.Connect.call(:delete, id)
  end

  @doc """
  Allow a worker to request more time to work on a job.
  This is useful for jobs that potentially take a long time, but you still want
  the benefits of a TTR pulling a job away from an unresponsive worker.  A worker
  may periodically tell the server that it's still alive and processing a job
  (e.g. it may do this on DEADLINE_SOON). The command postpones the auto
  release of a reserved job until TTR seconds from when the command is issued.
  """

  @spec touch(non_neg_integer) :: :touched | :not_found
  def touch(id) do
    Beanstalk.Connect.call(:touch, id)
  end

  @doc """
  Let the client inspect a job in the system. Peeking the given job id
  """

  @spec peek(non_neg_integer) :: {:found, non_neg_integer} | :not_found
  def peek(id) do
    Beanstalk.Connect.call(:peek, id)
  end

  @doc """
  Peeking the next ready job.
  """

  @spec peek_ready() :: {:found, non_neg_integer} | :not_found
  def peek_ready do
    Beanstalk.Connect.call(:peek_ready)
  end

  @doc """
  Peeking the delayed job with the shortest delay left.
  """

  @spec peek_delayed() :: {:found, non_neg_integer} | :not_found
  def peek_delayed do
    Beanstalk.Connect.call(:peek_delayed)
  end

  @doc """
  Peeking the next job in the list of buried jobs.
  """

  @spec peek_buried() :: {:found, non_neg_integer} | :not_found
  def peek_buried do
    Beanstalk.Connect.call(:peek_buried)
  end

  @doc """
  Move jobs into the ready queue. If there are any buried jobs, it will only kick buried jobs.
  Otherwise it will kick delayed jobs.

  Apply only to the currently used tube.
  """

  @spec kick(non_neg_integer) :: {:kicked, non_neg_integer}
  def kick(bound) do
    Beanstalk.Connect.call(:kick, bound)
  end

  @doc """
  Similar to `kick(bound)`, if the given job id exists and is in a buried or
  delayed state, it will be moved to the ready queue of the the same tube where it
  currently belongs.
  """

  @spec kick_job(non_neg_integer) :: :kicked | :not_found
  def kick_job(id) do
    Beanstalk.Connect.call(:kick_job, id)
  end

  @doc """
  Give statistical information about the system as a whole.
  """

  @spec stats() :: Keyword
  def stats do
    Beanstalk.Connect.call(:stats)
  end

  @doc """
  Similar to `stats/0`, gives statistical information about the specified job if
  it exists.
  """

  @spec stats_job(non_neg_integer) :: Keyword | :not_found
  def stats_job(id) do
    Beanstalk.Connect.call(:stats_job, id)
  end

  @doc """
  Similar to `stats/0`, gives statistical information about the specified tube
  if it exists.
  """

  @spec stats_tube(bitstring) :: Keyword | :not_found
  def stats_tube(tube) do
    Beanstalk.Connect.call(:stats_tube, tube)
  end

  @doc """
  Return a list of all existing tubes in the server.
  """

  @spec list_tubes() :: List
  def list_tubes do
    Beanstalk.Connect.call(:list_tubes)
  end

  @doc """
  Return the tube currently being used by the client.
  """

  @spec list_tube_used() :: {:using, bitstring}
  def list_tube_used do
    Beanstalk.Connect.call(:list_tube_used)
  end

  @doc """
  Return the tubes currently being watched by the client.
  """

  @spec list_tubes_watched() :: List
  def list_tubes_watched do
    Beanstalk.Connect.call(:list_tubes_watched)
  end

  @doc """
  Get a job from the currently watched tubes.
  """

  @spec reserve() :: {:reserved, non_neg_integer, bitstring}
  def reserve do
    Beanstalk.Connect.call_forever(:reserve)
  end

  @doc """
  Get a job from the currently watched tubes with timeout of seconds.
  """

  @spec reserve(non_neg_integer) :: {:reserved, non_neg_integer, bitstring} | :deadline_soon | :timed_out
  def reserve(timeout) do
    Beanstalk.Connect.call_forever(:reserve_with_timeout, timeout)
  end

  @doc """
  Put a job into the "buried" state. Buried jobs are put into a
  FIFO linked list and will not be touched by the server again until a client
  kicks them with the `kick` command.
  """

  @spec bury(non_neg_integer) :: :buried | :not_found
  @spec bury(non_neg_integer, non_neg_integer) :: :buried | :not_found
  def bury(id, pri \\ 0) do
    Beanstalk.Connect.call(:bury, id, pri)
  end

  @doc """
  Delay any new job being reserved for a given time.
  """

  @spec pause_tube(bitstring, non_neg_integer) :: :paused | :not_found
  def pause_tube(tube, delay) do
    Beanstalk.Connect.call(:pause_tube, tube, delay)
  end

  @doc """
  Put a reserved job back into the ready queue (and marks its state as "ready")
  to be run by any client. It is normally used when the job fails because of a transitory error.

  The opts can any combination of
  `:pri`    : a new priority to assign to the job;
  `:delay`  : an integer number of seconds to wait before putting the job back in the ready queue.
              The job will be in the "delayed" state during this time.
  """

  @spec release(non_neg_integer) :: :released | :buried | :not_found
  @spec release(non_neg_integer, Keyword) :: :released | :buried | :not_found
  def release(id, opts \\ []) do
    Beanstalk.Connect.call(:release, id, opts)
  end

end
