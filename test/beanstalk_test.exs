defmodule BeanstalkTest do
  use ExUnit.Case

  setup do
    # IO.puts "connect to beanstalkd first"
    {:ok, _} = Beanstalk.connect "192.168.10.61"
    on_exit fn -> Beanstalk.quit end
    :ok
  end

  test "`put`" do
    {:inserted, id1} = Beanstalk.put "hello world"
    assert is_integer(id1) == true

    {:inserted, id2} = Beanstalk.put("hello world", pri: 1)
    assert is_integer(id2) == true and id2 - id1 == 1

    {:inserted, id3} = Beanstalk.put "hello world", pri: 1, delay: 5
    assert is_integer(id3) == true and id3 - id1 == 2

    {:inserted, id4} = Beanstalk.put "hello world", pri: 1, delay: 5, ttr: 120
    assert is_integer(id4) == true and id4 - id1 == 3

    for id <- [id1, id2, id3, id4], do: Beanstalk.delete(id)
  end

  test "`use`" do
    tube = "not_default"
    assert {:using, data} = Beanstalk.use tube
    assert data == tube
  end

  test "`ignore`" do
    tube = "new_tube"
    ret = Beanstalk.ignore "default"
    assert ret == :not_ignored

    ret = Beanstalk.ignore tube
    assert elem(ret, 0) == :watching
    {:watching, count1} = Beanstalk.watch tube
    {:watching, count2} = Beanstalk.ignore tube
    assert count1 - count2 == 1
  end

  # test "`list_tubes`" do
  #   tubes = Beanstalk.list_tubes
  #   assert length(tubes) > 0
  # end

  # test "`list_tubes_watched`" do
  #   used_tube = Beanstalk.list_tubes_watched
  #   assert length(used_tube) == 1
  #   assert hd(used_tube) == "default"
  # end

  # test "`list_tube_used`" do
  #   used_tubes = Beanstalk.list_tubes_watched
  #   assert length(used_tubes) == 1
  #   assert hd(used_tubes) == "default"
  # end

  test "`watch`" do
    tube = "new_tube"
    assert {:watching, count} = Beanstalk.watch tube
    assert count == 2
  end

  test "`delete`" do
    {:inserted, id} = Beanstalk.put "put to ensure has jobs"
    ret = Beanstalk.delete id
    assert ret == :deleted

    {:inserted, id} = Beanstalk.put "another job"
    {:reserved, id, _} = Beanstalk.reserve 0
    assert Beanstalk.delete(id) == :not_found
  end

end
