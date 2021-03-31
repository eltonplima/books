defmodule Chapter4.Todo.TodoEntryTest do
  use ExUnit.Case

  alias Chapter4.Todo.TodoEntry

  @moduletag :capture_log

  doctest TodoEntry

  test "module exists" do
    assert is_list(TodoEntry.module_info())
  end

  test "new method" do
    todo_entry = TodoEntry.new(%{date: ~D[2021-03-07], title: "Shopping"})
    assert todo_entry == %TodoEntry{date: ~D[2021-03-07], title: "Shopping"}
  end
end
