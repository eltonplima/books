defmodule EnumStreamPractices do
  @moduledoc false

  def large_lines!(path, line_length \\ 80) do
    File.stream!(path)
    |> Stream.map(&String.replace(&1, "\n", ""))
    |> Enum.filter(&(String.length(&1) > line_length))
  end

  def lines_lengths!(path) do
    Enum.map(large_lines!(path, 0), fn string -> String.length(string) end)
  end

  def longest_line_length!(path) do
    large_lines!(path, 0)
    |> Enum.map(&String.length(&1))
    |> Enum.max()
  end

  def longest_line!(path) do
    large_lines!(path, 0)
    |> Enum.max(&(String.length(&1) > String.length(&2)))
  end

  def words_per_line!(path) do
    large_lines!(path, 0)
    |> Enum.map(&String.split(&1))
    |> Enum.map(&Enum.count(&1))
  end
end
