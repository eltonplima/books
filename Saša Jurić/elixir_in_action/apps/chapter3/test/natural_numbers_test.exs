defmodule NaturalNumbersTest do
  use ExUnit.Case
  doctest NaturalNumbers

  test "get zero natural number" do
    assert NaturalNumbers.first_n(0) == []
  end

  test "get one natural number" do
    assert NaturalNumbers.first_n(1) == [1]
  end

  test "get two natural number" do
    assert NaturalNumbers.first_n(2) == [1, 2]
  end

  test "with negative parameter" do
    assert NaturalNumbers.first_n(-1) == []
  end
end
