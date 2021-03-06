defmodule Chapter3.RangeTest do
  use ExUnit.Case

  test "from 1 to -4" do
    assert Chapter3.Range.range(1, -4) == [-4, -3, -2, -1, 0, 1]
  end

  test "from 1 to 1" do
    assert Chapter3.Range.range(1, 1) == [1]
  end

  test "from 1 to 2" do
    assert Chapter3.Range.range(1, 2) == [1, 2]
  end

  test "from 0 to 1" do
    assert Chapter3.Range.range(0, 1) == [0, 1]
  end

  test "from 1 to 10" do
    assert Chapter3.Range.range(1, 10) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  end
end
