defmodule PositiveTest do
  use ExUnit.Case

  test "only positive numbers" do
    assert Positive.positive([1, 2, 3]) == [1, 2, 3]
  end

  test "one negative number" do
    assert Positive.positive([-1, 2, 3]) == [2, 3]
  end

  test "only negative numbers" do
    assert Positive.positive([-1, -2, -3]) == []
  end
end
