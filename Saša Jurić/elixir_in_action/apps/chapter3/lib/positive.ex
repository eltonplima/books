defmodule Positive do
  def positive(list) do
    do_positive([], Enum.reverse(list))
  end

  defp do_positive(positives, []) do
    positives
  end

  defp do_positive(positives, [head | tail]) when head > 0 do
    do_positive([head | positives], tail)
  end

  defp do_positive(positives, [_head | tail]) do
    do_positive(positives, tail)
  end
end
