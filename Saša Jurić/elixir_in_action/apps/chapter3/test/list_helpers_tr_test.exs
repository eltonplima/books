defmodule ListHelpersTRTest do
  use ExUnit.Case

  test "[list_len_tr]with empty list" do
    assert ListHelpersTR.list_len_tr([]) == 0
  end

  test "[list_len_tr]with one element list" do
    assert ListHelpersTR.list_len_tr([10]) == 1
  end

  test "[list_len_tr]with many element list" do
    assert ListHelpersTR.list_len_tr([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) == 10
  end
end
