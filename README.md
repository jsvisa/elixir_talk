# ElixirTalk

## Overview
`ElixirTalk` is an Elixir client for [beanstalkd](http://kr.github.com/beanstalkd/).
It supported all commands defined in [ElixirTalk Protocol](https://raw.github.com/kr/beanstalkd/master/doc/protocol.txt)

## Installation & Setup
Clone this repository and then compile the project:

    $ git clone https://github.com/jsvisa/elixir_talk
    $ cd elixir_talk
    $ make

## Getting Started
You'll need **beanstalkd** listening at the host: `10.1.1.5`, and port: `14711`, simply start it with:
`$ beanstalkd -l 10.1.1.5 -p 14711`

Start the interactive shell and run the `ElixirTalk.connect` method to run it:

    iex -S mix
    iex(1)> {:ok, pid} = ElixirTalk.connect("10.1.1.5", 14711)
If you leave out the two arguments, `"127.0.0.1"` and `11300` are the default value, also there is a `timeout` parameter
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

    iex(19)> ElixirTalk.stats_job(pid, 27)
    [id: 27, tube: "default", state: "ready", pri: 0, age: 379859, delay: 0,
     ttr: 60, "time-left": 0, file: 0, reserves: 2, timeouts: 2, releases: 0,
     buries: 0, kicks: 0]

You can't access a deleted or not existed job's stats, or you'll only get a `:not_found`.

    iex(20)> ElixirTalk.stats_job(pid, 26)
    :not_found

You can also access a tube's statistics:

    iex(21)> ElixirTalk.stats_tube(pid, "default")
    [name: "default", "current-jobs-urgent": 19, "current-jobs-ready": 19,
     "current-jobs-reserved": 0, "current-jobs-delayed": 0,
     "current-jobs-buried": 0, "total-jobs": 37, "current-using": 1,
     "current-watching": 1, "current-waiting": 0, "cmd-delete": 18,
     "cmd-pause-tube": 0, pause: 0, "pause-time-left": 0]

Finally, there's an abundant amount of server-level statistics accessible via the Connection's stats method:

    iex(22)> ElixirTalk.stats(pid)
    ["current-jobs-urgent": 22, "current-jobs-ready": 28,
     "current-jobs-reserved": 0, "current-jobs-delayed": 0,
     "current-jobs-buried": 0, "cmd-put": 49, "cmd-peek": 0, "cmd-peek-ready": 13,
     "cmd-peek-delayed": 4, "cmd-peek-buried": 4, "cmd-reserve": 56,
     "cmd-reserve-with-timeout": 41, "cmd-delete": 22, "cmd-release": 0,
     "cmd-use": 14, "cmd-watch": 7, "cmd-ignore": 8, "cmd-bury": 0, "cmd-kick": 0,
     "cmd-touch": 0, "cmd-stats": 21, "cmd-stats-job": 8, "cmd-stats-tube": 13,
     "cmd-list-tubes": 16, "cmd-list-tube-used": 17, "cmd-list-tubes-watched": 20,
     "cmd-pause-tube": 0, "job-timeouts": 47, "total-jobs": 47,
     "max-job-size": 65535, "current-tubes": 3, "current-connections": 1,
     "current-producers": 0, "current-workers": 0, "current-waiting": 0,
     "total-connections": 90, pid: 17492, version: 1.8, "rusage-utime": 15.396318,
     "rusage-stime": 2312.522858, uptime: 454596, "binlog-oldest-index": 0,
     "binlog-current-index": 0, "binlog-records-migrated": 0,
     "binlog-records-written": 0, "binlog-max-size": 10485760]


