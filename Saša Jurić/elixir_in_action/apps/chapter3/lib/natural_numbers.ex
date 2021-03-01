defmodule NaturalNumbers do
  @spec first_n(non_neg_integer) :: list
  def first_n(nth) when nth <= 0 do
    []
  end

  def first_n(nth) when nth > 0 do
    Enum.reverse([nth | first_n(nth - 1)])
  end
end
