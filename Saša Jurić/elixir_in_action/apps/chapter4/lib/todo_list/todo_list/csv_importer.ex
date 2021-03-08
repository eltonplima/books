defmodule Chapter4.TodoList.CsvImporter do
  @moduledoc false

  alias Chapter4.TodoList.TodoList

  def import(csv_file_path) do
    csv_file_path
    |> file_to_list_of_entries()
    |> TodoList.new()
  end

  defp file_to_list_of_entries(csv_file_path) do
    File.stream!(csv_file_path)
    |> Stream.map(&sanitize(&1))
    |> Stream.map(&extract_fields(&1))
    |> Stream.map(&parse_fields(&1))
    |> Stream.transform([], fn entry, acc -> {[entry], acc} end)
    |> Enum.map(& &1)
  end

  defp sanitize(line) do
    line
    |> String.trim()
    |> String.replace("\n", "")
  end

  defp extract_fields(line) do
    line
    |> String.split(",")
  end

  defp parse_fields([date, title]) do
    [year, month, day] =
      date
      |> String.split("/")
      |> Enum.map(&String.to_integer(&1))

    %{date: Date.new!(year, month, day), title: title}
  end
end
