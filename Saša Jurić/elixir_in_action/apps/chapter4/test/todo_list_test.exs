defmodule Chapter4.TodoList.TodoListTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  alias Chapter4.TodoList.TodoList
  alias Chapter4.TodoList.TodoEntry

  doctest Chapter4.TodoList.TodoList

  defmodule AddEntryTest do
    use ExUnit.Case, async: true

    test "add entries in sequence" do
      todo_list =
        TodoList.new()
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-01], title: "Test 1"})
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-02], title: "Test 2"})

      assert ^todo_list = %TodoList{
               auto_id: 3,
               entries: %{
                 2 => %TodoEntry{id: 2, date: ~D[2021-01-02], title: "Test 2"},
                 1 => %TodoEntry{id: 1, date: ~D[2021-01-01], title: "Test 1"}
               }
             }
    end

    test "add_entry with invalid todo_list type raises FunctionClauseError" do
      assert_raise FunctionClauseError, fn ->
        TodoList.add_entry(%{}, %TodoEntry{date: ~D[2021-01-01], title: "Test 1"})
      end
    end
  end

  defmodule EntriesTest do
    use ExUnit.Case, async: true

    test "find many entries on the same date" do
      todo_list =
        TodoList.new()
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-01], title: "Test 1"})
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-02], title: "Test 2"})
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-02], title: "Test 3"})

      assert TodoList.entries(todo_list, ~D[2021-01-02]) == [
               %TodoEntry{id: 2, date: ~D[2021-01-02], title: "Test 2"},
               %TodoEntry{id: 3, date: ~D[2021-01-02], title: "Test 3"}
             ]
    end

    test "find single entry by date" do
      todo_list =
        TodoList.new()
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-01], title: "Test 1"})
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-02], title: "Test 2"})
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-02], title: "Test 3"})

      assert TodoList.entries(todo_list, ~D[2021-01-01]) == [
               %TodoEntry{id: 1, date: ~D[2021-01-01], title: "Test 1"}
             ]
    end
  end

  defmodule UpdateEntryTest do
    use ExUnit.Case, async: true

    test "update an existing entry" do
      todo_list =
        TodoList.new()
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-01], title: "Test 1"})

      assert TodoList.update_entry(todo_list, 1, fn entry ->
               %{entry | title: "Message was updated"}
             end) == %TodoList{
               auto_id: 2,
               entries: %{
                 1 => %TodoEntry{id: 1, date: ~D[2021-01-01], title: "Message was updated"}
               }
             }
    end

    test "cannot change the id field" do
      todo_list =
        TodoList.new()
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-01], title: "Test 1"})

      assert_raise MatchError, fn ->
        TodoList.update_entry(todo_list, 1, fn entry ->
          %{entry | id: 2}
        end)
      end
    end
  end

  defmodule DeleteEntryTest do
    use ExUnit.Case, async: true

    test "delete from an empty todo list" do
      assert %TodoList{auto_id: 1, entries: %{}} == TodoList.delete_entry(TodoList.new(), 1)
    end

    test "delete an entry when the todo list has only one item" do
      todo_list =
        TodoList.new()
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-01], title: "Test 1"})

      assert %TodoList{auto_id: 2, entries: %{}} == TodoList.delete_entry(todo_list, 1)
    end

    test "delete an entry when the todo list has many items" do
      todo_list =
        TodoList.new()
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-01], title: "Test 1"})
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-02], title: "Test 2"})

      assert %TodoList{
               auto_id: 3,
               entries: %{2 => %TodoEntry{id: 2, date: ~D[2021-01-02], title: "Test 2"}}
             } == TodoList.delete_entry(todo_list, 1)
    end
  end

  defmodule NewTest do
    use ExUnit.Case, async: true

    test "create a new todo list based on a raw list of TODOs" do
      raw_todo_list = [
        %{date: ~D[2021-03-06], title: "Dentist"},
        %{date: ~D[2021-03-07], title: "Shopping"},
        %{date: ~D[2021-03-08], title: "Movies"}
      ]

      assert TodoList.new(raw_todo_list) == %TodoList{
               auto_id: 4,
               entries: %{
                 3 => %TodoEntry{id: 3, date: ~D[2021-03-08], title: "Movies"},
                 2 => %TodoEntry{id: 2, date: ~D[2021-03-07], title: "Shopping"},
                 1 => %TodoEntry{id: 1, date: ~D[2021-03-06], title: "Dentist"}
               }
             }
    end
  end

  defmodule ProtocolsImplementationTest do
    use ExUnit.Case, async: true

    test "TodoList implements String.Chars protocol" do
      assert capture_io(fn -> IO.puts(TodoList.new()) end) == "#TodoList\n"
    end

    test "TodoList implements Collectable protocol" do
      expected =
        TodoList.new()
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-01], title: "Test 1"})
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-02], title: "Test 2"})
        |> TodoList.add_entry(%TodoEntry{date: ~D[2021-01-03], title: "Test 3"})

      entries = [
        %{date: ~D[2021-01-01], title: "Test 1"},
        %{date: ~D[2021-01-02], title: "Test 2"},
        %{date: ~D[2021-01-03], title: "Test 3"}
      ]

      assert expected == for(entry <- entries, into: TodoList.new(), do: entry)
    end
  end
end
