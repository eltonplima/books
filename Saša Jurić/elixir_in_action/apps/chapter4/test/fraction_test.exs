defmodule FractionTest do
  use ExUnit.Case

  alias Fraction

  @moduletag :capture_log

  doctest Fraction

  test "new returns struct" do
    assert %Fraction{} = Fraction.new(1, 2)
  end

  test "value returns a decimal representation of the fraction" do
    expected = 1 / 2
    assert ^expected = Fraction.new(1, 2) |> Fraction.value()
  end

  test "add two fractions" do
    assert Fraction.new(6, 8) == Fraction.add(Fraction.new(1, 2), Fraction.new(1, 4))
  end
end
