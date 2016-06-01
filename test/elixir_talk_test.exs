defmodule ElixirTalkTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = ElixirTalk.connect '192.168.99.10', 11300

    {m, s, ms} = :os.timestamp
    tube = "ElixirTalk_#{m}_#{s}_#{ms}"
    ElixirTalk.use pid, tube
    ElixirTalk.watch pid, tube
    ElixirTalk.ignore pid, "default"

    # Don't have to manually stop it, because ExUnit will send `shutdown` to the runner
    # on_exit fn -> ElixirTalk.quit(pid) end
    {:ok, [tube: tube, pid: pid]}
  end

  test "`connect` domain" do
    {:ok, pid} = ElixirTalk.connect 'my.beanstalkd.com', 11300
    assert is_pid(pid)
  end

  test "`put`", ctx do
    {:inserted, id1} = ElixirTalk.put ctx[:pid], "hello world"
    assert is_integer(id1)

    {:inserted, id2} = ElixirTalk.put(ctx[:pid], "hello world", pri: 1)
    assert is_integer(id2) == true and id2 - id1 == 1

    {:inserted, id3} = ElixirTalk.put ctx[:pid], "hello world", pri: 1, delay: 5
    assert is_integer(id3) == true and id3 - id1 == 2

    {:inserted, id4} = ElixirTalk.put ctx[:pid], "hello world", pri: 1, delay: 5, ttr: 120
    assert is_integer(id4) == true and id4 - id1 == 3

    for id <- [id1, id2, id3, id4], do: ElixirTalk.delete(ctx[:pid], id)
  end

  test "`put` unicode", ctx do
    {:inserted, id} = ElixirTalk.put ctx[:pid], "hełło"
    assert is_integer(id)
    ElixirTalk.delete(ctx[:pid], id)
  end

  test "`use`", ctx do
    {:using, now_tube} = ElixirTalk.list_tube_used(ctx[:pid])
    tube = "not_default"
    {:using, data} = ElixirTalk.use ctx[:pid], tube

    assert data == tube
    ElixirTalk.use ctx[:pid], now_tube
  end

  test "`reserved`", ctx do
    str = "hello world"
    {:inserted, put_id} = ElixirTalk.put ctx[:pid], str
    {:reserved, expected_id, expected_str} = ElixirTalk.reserve ctx[:pid]
    assert put_id == expected_id
    assert str == expected_str
  end

  test "`ignore`", ctx do
    ret = ElixirTalk.ignore ctx[:pid], ctx[:tube]
    assert ret == :not_ignored

    tube = "new_tube"
    {watch, count} = ElixirTalk.ignore ctx[:pid], tube
    assert watch == :watching && count == 1
  end

  test "`list_tubes`", ctx do
    tubes = ElixirTalk.list_tubes(ctx[:pid])
    assert length(tubes) > 0
  end

  test "`list_tubes_watched`", ctx do
    used_tube = ElixirTalk.list_tubes_watched ctx[:pid]
    assert length(used_tube) == 1
    assert hd(used_tube) == ctx[:tube]

    tubes = ~w(tube1 tube2 tube3 tube4 tube5)
    Enum.each tubes, &(ElixirTalk.watch ctx[:pid], &1)
    used_tube = ElixirTalk.list_tubes_watched ctx[:pid]
    assert length(used_tube) == 1 + length(tubes)
    assert hd(used_tube) == ctx[:tube]
  end

  test "`list_tube_used`", ctx do
    {:using, used_tube} = ElixirTalk.list_tube_used ctx[:pid]
    assert used_tube == ctx[:tube]

    using_tube = "using_tube_foo"
    ElixirTalk.use ctx[:pid], using_tube
    {:using, used_tube} = ElixirTalk.list_tube_used ctx[:pid]
    assert used_tube == using_tube
  end

  test "`watch`", ctx do
    tube = "new_tube"
    {:watching, count} = ElixirTalk.watch ctx[:pid], tube
    assert count == 2
  end

  test "`delete`", ctx do
    {:inserted, id} = ElixirTalk.put ctx[:pid], "put to ensure has jobs"
    ret = ElixirTalk.delete ctx[:pid], id
    assert ret == :deleted

    ret = ElixirTalk.delete ctx[:pid], id
    assert ret == :not_found

    {:inserted, id} = ElixirTalk.put ctx[:pid], "another job"
    {:reserved, ^id, _} = ElixirTalk.reserve ctx[:pid], 0
    assert ElixirTalk.delete(ctx[:pid], id) == :deleted
  end

  test "with large body smaller than 2**16", ctx do
    body = String.duplicate("{msg: \"hello world\"}", 500)
    {:inserted, id} = ElixirTalk.put ctx[:pid], body
    assert is_integer(id)
    {:reserved, ^id, ^body} = ElixirTalk.reserve ctx[:pid], 0
    assert ElixirTalk.delete(ctx[:pid], id) == :deleted
  end

  test "with large body larger than 2**16", ctx do
    body = String.duplicate("{msg: \"hello world\"}", 5000)
    assert :job_too_big == ElixirTalk.put ctx[:pid], body
  end

  test "`stats`", ctx do
    stats = ElixirTalk.stats(ctx[:pid])
    assert is_map(stats)

    keys = ["current-jobs-urgent", "cmd-peek", "uptime", "cmd-list-tubes-watched",
            "rusage-utime", "cmd-release", "binlog-current-index", "cmd-watch",
            "total-connections", "current-workers", "current-waiting", "cmd-ignore", "id",
            "cmd-put", "job-timeouts", "cmd-stats-tube", "max-job-size",
            "current-producers", "current-jobs-buried", "cmd-touch", "cmd-kick",
            "current-tubes", "cmd-bury", "current-jobs-ready", "cmd-stats",
            "cmd-list-tube-used", "version", "binlog-records-migrated", "hostname",
            "binlog-records-written", "current-jobs-reserved", "cmd-peek-ready",
            "cmd-pause-tube", "current-jobs-delayed", "cmd-peek-buried", "cmd-use",
            "cmd-reserve", "current-connections", "rusage-stime",
            "cmd-reserve-with-timeout", "binlog-oldest-index", "pid", "binlog-max-size",
            "total-jobs", "cmd-delete", "cmd-list-tubes", "cmd-stats-job",
            "cmd-peek-delayed"]
          |> Enum.into(MapSet.new)
    assert MapSet.equal?(Enum.into(Map.keys(stats), MapSet.new), keys)
  end

  test "`stats_job`", ctx do
    {:inserted, id} = ElixirTalk.put ctx[:pid], "put to ensure has jobs"

    stats = ElixirTalk.stats_job(ctx[:pid], id)
    assert is_map(stats)

    keys = ["id", "tube", "state", "pri", "age", "delay", "ttr", "time-left",
             "file", "reserves", "timeouts", "releases", "buries", "kicks"]
          |> Enum.into(MapSet.new)
    assert MapSet.equal?(Enum.into(Map.keys(stats), MapSet.new), keys)

    ElixirTalk.delete(ctx[:pid], id)
  end

  test "`stats_tube`", ctx do
    stats = ElixirTalk.stats_tube(ctx[:pid], ctx[:tube])
    assert is_map(stats)

    keys = ["cmd-delete", "cmd-pause-tube", "current-jobs-buried", "current-jobs-delayed",
            "current-jobs-ready", "current-jobs-reserved", "current-jobs-urgent",
            "current-using", "current-waiting", "current-watching", "name", "pause",
            "pause-time-left", "total-jobs"]
          |> Enum.into(MapSet.new)
    assert MapSet.equal?(Enum.into(Map.keys(stats), MapSet.new), keys)

    assert :not_found == ElixirTalk.stats_tube(ctx[:pid], ctx[:tube] <> "_notfound")
  end

end
