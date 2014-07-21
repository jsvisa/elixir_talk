defmodule BeanstalkTest do
  use ExUnit.Case

  setup do
    # IO.puts "connect to beanstalkd first"
    {:ok, pid} = Beanstalk.connect "192.168.10.61"
    on_exit fn -> Beanstalk.quit(pid) end
    {:ok, [pid: pid]}
  end

  test "`put`", ctx do
    {:inserted, id1} = Beanstalk.put ctx[:pid], "hello world"
    assert is_integer(id1) == true

    {:inserted, id2} = Beanstalk.put(ctx[:pid], "hello world", pri: 1)
    assert is_integer(id2) == true and id2 - id1 == 1

    {:inserted, id3} = Beanstalk.put ctx[:pid], "hello world", pri: 1, delay: 5
    assert is_integer(id3) == true and id3 - id1 == 2

    {:inserted, id4} = Beanstalk.put ctx[:pid], "hello world", pri: 1, delay: 5, ttr: 120
    assert is_integer(id4) == true and id4 - id1 == 3

    for id <- [id1, id2, id3, id4], do: Beanstalk.delete(ctx[:pid], id)
  end

  test "`use`", ctx do
    tube = "not_default"
    assert {:using, data} = Beanstalk.use ctx[:pid], tube
    assert data == tube
  end

  test "`ignore`", ctx do
    tube = "new_tube"
    ret = Beanstalk.ignore ctx[:pid], "default"
    assert ret == :not_ignored

    ret = Beanstalk.ignore ctx[:pid], tube
    assert elem(ret, 0) == :watching
    {:watching, count1} = Beanstalk.watch ctx[:pid], tube
    {:watching, count2} = Beanstalk.ignore ctx[:pid], tube
    assert count1 - count2 == 1
  end

  # test "`list_tubes`", ctx do
  #   tubes = Beanstalk.list_tubes(ctx[:pid])
  #   assert length(tubes) > 0
  # end

  # test "`list_tubes_watched`", ctx do
  #   used_tube = Beanstalk.list_tubes_watched ctx[:pid]
  #   assert length(used_tube) == 1
  #   assert hd(used_tube) == "default"
  # end

  # test "`list_tube_used`", ctx do
  #   used_tubes = Beanstalk.list_tubes_watched ctx[:pid]
  #   assert length(used_tubes) == 1
  #   assert hd(used_tubes) == "default"
  # end

  test "`watch`", ctx do
    tube = "new_tube"
    assert {:watching, count} = Beanstalk.watch ctx[:pid], tube
    assert count == 2
  end

  test "`delete`", ctx do
    {:inserted, id} = Beanstalk.put ctx[:pid], "put to ensure has jobs"
    ret = Beanstalk.delete ctx[:pid], id
    assert ret == :deleted

    {:inserted, id} = Beanstalk.put ctx[:pid], "another job"
    {:reserved, id, _} = Beanstalk.reserve ctx[:pid], 0
    assert Beanstalk.delete(ctx[:pid], id) == :not_found
  end

end
