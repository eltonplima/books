defmodule EnumStreamPracticesTest do
  use ExUnit.Case

  alias EnumStreamPractices

  @moduletag :capture_log

  doctest EnumStreamPractices

  setup_all do
    file_content = "This is a sample paragraph.
A
Simple
Word
Per
Line
AGreaterWord
This is a really long, long, long line of text to test. With more than 80 columns!"
    example_file_1 = "test/example.txt"
    File.write!(example_file_1, file_content, [:write, {:encoding, :utf8}])
    on_exit(fn -> File.rm!(example_file_1) end)
    {:ok, text_file_path: example_file_1}
  end

  test "get lines with length greater than the default 80 columns", state do
    IO.inspect(state[:text_file_path])

    assert EnumStreamPractices.large_lines!(state[:text_file_path]) == [
             "This is a really long, long, long line of text to test. With more than 80 columns!"
           ]
  end

  test "get lines with length greater than the custom 10 columns", state do
    assert EnumStreamPractices.large_lines!(state[:text_file_path], 10) == [
             "This is a sample paragraph.",
             "AGreaterWord",
             "This is a really long, long, long line of text to test. With more than 80 columns!"
           ]
  end

  test "get lines lengths", state do
    assert EnumStreamPractices.lines_lengths!(state[:text_file_path]) == [
             27,
             1,
             6,
             4,
             3,
             4,
             12,
             82
           ]
  end

  test "get longest line length", state do
    assert EnumStreamPractices.longest_line_length!(state[:text_file_path]) == 82
  end

  test "get longest line", state do
    assert EnumStreamPractices.longest_line!(state[:text_file_path]) ==
             "This is a really long, long, long line of text to test. With more than 80 columns!"
  end

  test "words per line", state do
    assert EnumStreamPractices.words_per_line!(state[:text_file_path]) == [
             5,
             1,
             1,
             1,
             1,
             1,
             1,
             17
           ]
  end
end
