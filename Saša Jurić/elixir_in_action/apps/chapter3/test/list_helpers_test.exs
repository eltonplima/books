defmodule ListHelpersTest do
  use ExUnit.Case

  test "[list_len]with empty list" do
    assert ListHelpers.list_len([]) == 0
  end

  test "[list_len]with one element list" do
    assert ListHelpers.list_len([10]) == 1
  end

  test "[list_len]with many element list" do
    assert ListHelpers.list_len([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) == 10
  end
end
