defmodule BeanstalkTest do
  use ExUnit.Case

  setup do
    # IO.puts "connect to beanstalkd first"
    {:ok, _} = Beanstalk.connect "10.1.1.5"
    :ok
  end

  teardown do
    Beanstalk.quit
  end

  test "`put`" do
    assert {:inserted, id1} = Beanstalk.put "hello world"
    assert is_integer(id1) == true

    assert {:inserted, id2} = Beanstalk.put("hello world", pri: 1)
    assert is_integer(id2) == true and id2 - id1 == 1

    assert {:inserted, id3} = Beanstalk.put "hello world", pri: 1, delay: 5
    assert is_integer(id3) == true and id3 - id1 == 2

    assert {:inserted, id4} = Beanstalk.put "hello world", pri: 1, delay: 5, ttr: 120
    assert is_integer(id4) == true and id4 - id1 == 3
  end

  test "`use`" do
    assert tube = "not_default"
    assert {:using, data} = Beanstalk.use tube
    assert data == tube
  end

  test "`ignore`" do
    assert tube = "new_tube"
    assert ret = Beanstalk.ignore "default"
    assert ret == :not_ignored

    assert ret = Beanstalk.ignore tube
    assert elem(ret, 0) == :watching
    assert {:watching, count1} = Beanstalk.watch tube
    assert {:watching, count2} = Beanstalk.ignore tube
    assert count1 - count2 == 1
  end

  test "`list_tubes`" do
    assert tubes = Beanstalk.list_tubes
    assert length(tubes) > 0
  end

  test "`list_tubes_watched`" do
    assert used_tube = Beanstalk.list_tubes_watched
    assert length(used_tube) == 1
    assert hd(used_tube) == "default"
  end

  test "`list_tube_used`" do
    assert used_tubes = Beanstalk.list_tubes_watched
    assert length(used_tubes) == 1
    assert hd(used_tubes) == "default"
  end

  test "`watch`" do
    assert tube = "new_tube"
    assert {:watching, count} = Beanstalk.watch tube
    assert count == 2
  end

  test "`delete`" do
    Beanstalk.put "put to ensure has jobs"
    assert {:reserved, id, _} = Beanstalk.reserve 0

    assert ret = Beanstalk.delete id
    assert ret == :deleted

    assert ret = Beanstalk.delete id
    assert ret == :not_found
  end

end
