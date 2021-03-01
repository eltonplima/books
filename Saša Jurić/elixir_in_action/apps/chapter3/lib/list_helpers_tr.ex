defmodule ListHelpersTR do
  def list_len_tr(list) do
    calculate_list_len_tr(0, list)
  end

  defp calculate_list_len_tr(length, []) do
    length
  end

  defp calculate_list_len_tr(length, [_ | tail]) do
    calculate_list_len_tr(length + 1, tail)
  end
end
