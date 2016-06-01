# ElixirTalk

## Overview
`ElixirTalk` is an Elixir client for [beanstalkd](http://kr.github.com/beanstalkd/).
It supported all commands defined in [Beanstalkd Protocol](https://raw.github.com/kr/beanstalkd/master/doc/protocol.txt)

## Installation & Setup

First, add ElixirTalk to your `mix.exs` dependencies:

```elixir
def deps do
  [{:elixir_talk, "~> 1.1"}]
end
```

and run `$ mix deps.get`.

## Getting Started
You'll need **beanstalkd** listening at the host: `10.1.1.5`, and port: `14711`, simply start it with:
`$ beanstalkd -l 10.1.1.5 -p 14711`

Start the interactive shell and run the `ElixirTalk.connect` method to run it:

    iex -S mix
    iex(1)> {:ok, pid} = ElixirTalk.connect('10.1.1.5', 14711)
If you leave out the two arguments, `'127.0.0.1'` and `11300` are the default value, also there is a `timeout` parameter
which determines how long, **in millliseconds**, the socket will wait for **beanstalkd** to respond to its' initial
connection, default to `:infinity`.

## Basic Operation
After connection to the beanstalkd successfully, we can enqueue our jobs:

    iex(2)> ElixirTalk.put(pid, "hello world")
    {:inserted, 1}

Or we can get jobs:

    iex(3)> ElixirTalk.reserve(pid)
    {:reserved, 1, "hello world"}

Once we are finishing a job, we have to delete it, otherwise jobs are re-queued by **beanstalkd**
after a `:ttr` "time to run" (60 seconds, per default) is surpassed. A job is marked as finished, by calling delete:

    iex(4)> ElixirTalk.delete(pid, 1)
    :deleted

`reserve` blocks until a job is ready, possibly forever. We can invoke reserve with a timeout **in seconds**,
to indicate how long we want to wait to receive a job. If such a reserve times out, it will return `:timed_out`:

    iex(5)> ElixirTalk.reserve(pid, 5)
    :timed_out

If you use a timeout of 0, reserve will immediately return either a job or `:timed_out`.

## Tube Management

A single **beanstalkd** server can provide many different queues, called "tubes" in **beanstalkd**.
To see all available tubes:

    iex(6)> ElixirTalk.list_tubes(pid)
    ["default"]

A **beanstalkd** client can choose one tube into which its job are putted. This is the tube "used" by the client.
To see what tube you are currently using:

    iex(7)> ElixirTalk.list_tube_used(pid)
    {:using, "default"}

Unless told otherwise, a client uses the `"default"` tube. If you want to use a different tube:

    iex(8)> ElixirTalk.use(pid, "notDefault")
    {:using, "notDefault"}
    iex(8)> ElixirTalk.list_tube_used(pid)
    {:using, "notDefault"}

If you decide to use a tube which does not yet exist, the tube is automatically created by **beanstalkd**, so you can see
we initially used the `"default"` tube. Of course, you can always switch back to the default tube.
Tubes that don't have any client using or watching, be vanished automatically:

    iex(9)> ElixirTalk.list_tubes(pid)
    ["default", "notDefault"]
    iex(10)> ElixirTalk.use(pid, "default")
    {:using, "default"}
    iex(11)> ElixirTalk.list_tubes(pid)
    ["default"]

Further more, a beanstalkd client can choose many tubes to `reserve` jobs from. These tubes are `watched` by the client.
To see what tubes you are currently watching:

    iex(12)> ElixirTalk.list_tubes_watched(pid)
    ["default"]

To watch an additional tube:

    iex(13)> ElixirTalk.watch(pid, "notDefault")
    {:watching, 2}
    iex(14)> ElixirTalk.list_tubes_watched(pid)
    ["default", "notDefault"]

The same to `use`, tubes that do not yet exist are created automatically once you start watching them.

To stop `watch` a tube:

    iex(15)> ElixirTalk.ignore(pid, "default")
    {:watching, 1}
    iex(16)> ElixirTalk.list_tubes_watched(pid)
    ["notDefault"]

You can't watch zero tubes. So if you try to ignore the last tube you are watching, this is silently return `:not_ignored`:

    iex(17)> ElixirTalk.ignore(pid, "notDefault")
    :not_ignored
    iex(18)> ElixirTalk.list_tubes_watched(pid)
    ["notDefault"]

Note that `use` and `watch` these concerns are fully orthogonal: for example, when you use a tube, it is not
automatically watched. Neither does watching a tube affect the tube you are using. You may `use` a tube in one process
to put your jobs, while in another process you `watch` a job just to get the putted jobs.

## Statistics

**ElixirTalkd** accumulates various statistics at the server, tube and job level. Statistical details for a job can only be retrieved during the job's lifecycle. So let's create another job:

    %{"age" => 13, "buries" => 0, "delay" => 0, "file" => 0, "id" => 10,
      "kicks" => 0, "pri" => 0, "releases" => 0, "reserves" => 1, "state" => "reserved",
      "time-left" => 53, "timeouts" => 0, "ttr" => 60, "tube" => "default"}

You can't access a deleted or not existed job's stats, or you'll only get a `:not_found`.

    iex(20)> ElixirTalk.stats_job(pid, 26)
    :not_found

You can also access a tube's statistics:

    iex(21)> ElixirTalk.stats_tube(pid, "default")
    %{"cmd-delete" => 0, "cmd-pause-tube" => 0, "current-jobs-buried" => 0,
      "current-jobs-delayed" => 0, "current-jobs-ready" => 1,
      "current-jobs-reserved" => 0, "current-jobs-urgent" => 1,
      "current-using" => 1, "current-waiting" => 0, "current-watching" => 1,
      "name" => "default", "pause" => 0, "pause-time-left" => 0, "total-jobs" => 1}

Finally, there's an abundant amount of server-level statistics accessible via the Connection's stats method:

    iex(22)> ElixirTalk.stats(pid)
     %{"current-jobs-urgent" => 2, "cmd-peek" => 0, "uptime" => 1154,
       "cmd-list-tubes-watched" => 3, "rusage-utime" => 0.0, "cmd-release" => 0,
       "binlog-current-index" => 0, "cmd-watch" => 19, "total-connections" => 15,
       "current-workers" => 1, "current-waiting" => 0, "cmd-ignore" => 15,
       "id" => "def32f0744b36db5", "cmd-put" => 11, "job-timeouts" => 1,
       "cmd-stats-tube" => 3, "max-job-size" => 65535, "current-producers" => 1,
       "current-jobs-buried" => 0, "cmd-touch" => 0, "cmd-kick" => 0,
       "current-tubes" => 2, "cmd-bury" => 0, "current-jobs-ready" => 2,
       "cmd-stats" => 3, "cmd-list-tube-used" => 3, "version" => "1.10+4+g96e8756",
       "binlog-records-migrated" => 0, "hostname" => "v",
       "binlog-records-written" => 0, "current-jobs-reserved" => 0,
       "cmd-peek-ready" => 0, "cmd-pause-tube" => 0, "current-jobs-delayed" => 0,
       "cmd-peek-buried" => 0, "cmd-use" => 16, "cmd-reserve" => 2,
       "current-connections" => 1, "rusage-stime" => 0.014314,
       "cmd-reserve-with-timeout" => 2, "binlog-oldest-index" => 0, "pid" => 9987,
       "binlog-max-size" => 10485760, "total-jobs" => 10, "cmd-delete" => 9,
       "cmd-list-tubes" => 3, "cmd-stats-job" => 3, "cmd-peek-delayed" => 0}


## Test

If you want to run the TestCase, you set the correct Beanstalkd IP and Port
in [test/elixir_talk_test.exs]("http://www.github.com/jsvisa/elixit_talk/test/elixir_talk_test.exs"),
also you should set a hostname `my.beanstalkd.com` with the provided ip in */etc/hosts*

