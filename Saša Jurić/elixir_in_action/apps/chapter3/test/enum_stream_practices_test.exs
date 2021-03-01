defmodule EnumStreamPracticesTest do
  use ExUnit.Case

  alias EnumStreamPractices

  @moduletag :capture_log

  doctest EnumStreamPractices

  test "get lines with length greater than the default 80 columns" do
    assert EnumStreamPractices.large_lines!("test/fixtures/example.txt") == [
             "This is a really long, long, long line of text to test. With more than 80 columns!"
           ]
  end

  test "get lines with length greater than the custom 10 columns" do
    assert EnumStreamPractices.large_lines!("test/fixtures/example.txt", 10) == [
             "This is a sample paragraph.",
             "AGreaterWord",
             "This is a really long, long, long line of text to test. With more than 80 columns!"
           ]
  end
end
