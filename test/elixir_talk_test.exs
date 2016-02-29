defmodule ElixirTalkTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = ElixirTalk.connect '192.168.33.10', 11300

    {m, s, ms} = :os.timestamp
    tube = "ElixirTalk_#{m}_#{s}_#{ms}"
    ElixirTalk.use pid, tube
    ElixirTalk.watch pid, tube
    ElixirTalk.ignore pid, "default"

    on_exit fn -> ElixirTalk.quit(pid) end
    {:ok, [tube: tube, pid: pid]}
  end

  test "`connect`" do
    {:ok, pid} = ElixirTalk.connect 'my.beanstalkd.com', 11300

    on_exit fn -> ElixirTalk.quit(pid) end
  end

  test "`put`", ctx do
    {:inserted, id1} = ElixirTalk.put ctx[:pid], "hello world"
    assert is_integer(id1) == true

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
    {:reserved, expected_id, {byte, expected_str}} = ElixirTalk.reserve ctx[:pid]
    assert put_id == expected_id
    assert str == expected_str && byte == byte_size(str)
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

end
