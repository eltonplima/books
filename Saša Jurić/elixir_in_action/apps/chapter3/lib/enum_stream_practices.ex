defmodule EnumStreamPractices do
  @moduledoc false

  def large_lines!(path, length \\ 80) do
    File.stream!(path)
    |> Stream.map(&String.replace(&1, "\n", ""))
    |> Enum.filter(&(String.length(&1) > length))
  end
end
