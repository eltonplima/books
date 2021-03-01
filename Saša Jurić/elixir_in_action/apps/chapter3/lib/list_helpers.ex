defmodule ListHelpers do
  def list_len([]) do
    0
  end

  def list_len([_ | tail]) do
    calculate_list_len(1, tail)
  end

  defp calculate_list_len(size, []) do
    size
  end

  defp calculate_list_len(size, [_ | tail]) do
    calculate_list_len(size + 1, tail)
  end
end
