defmodule Chapter3.Range do
  def range(start, stop) when start > stop do
    do_range([], stop, start)
  end

  def range(start, stop) do
    do_range([], start, stop)
  end

  defp do_range(current_range, start, stop) when start > stop do
    Enum.reverse(current_range)
  end

  defp do_range(current_range, start, stop) do
    do_range([start | current_range], start + 1, stop)
  end
end
