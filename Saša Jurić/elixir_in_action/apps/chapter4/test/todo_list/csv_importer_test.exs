defmodule Chapter4.TodoList.CsvImporterTest do
  use ExUnit.Case

  alias Chapter4.TodoList.CsvImporter
  alias Chapter4.TodoList.TodoEntry
  alias Chapter4.TodoList.TodoList

  @moduletag :capture_log

  doctest CsvImporter

  test "module exists" do
    assert is_list(CsvImporter.module_info())
  end

  setup_all do
    csv_content = " 2021/03/06,Dentist   \n"
    csv_file_path = "test/single.csv"
    File.write!(csv_file_path, csv_content, [:write, {:encoding, :utf8}])
    on_exit(fn -> File.rm!(csv_file_path) end)
    {:ok, single_entry_csv_file_path: csv_file_path}
  end

  test "create a TodoList from imported from CSV file with single entry", state do
    csv_file_path = state[:single_entry_csv_file_path]
    todo_list = CsvImporter.import(csv_file_path)

    assert todo_list == %TodoList{
             auto_id: 2,
             entries: %{
               1 => %TodoEntry{id: 1, date: ~D[2021-03-06], title: "Dentist"}
             }
           }
  end
end