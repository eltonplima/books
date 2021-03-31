defmodule Chapter4.Todo.CsvImporterTest do
  use ExUnit.Case

  alias Chapter4.Todo.CsvImporter
  alias Chapter4.Todo.TodoEntry
  alias Chapter4.Todo.TodoList

  @moduletag :capture_log

  doctest CsvImporter

  test "module exists" do
    assert is_list(CsvImporter.module_info())
  end

  setup_all do
    single_line_csv_file_path = "test/single_line.csv"
    multi_line_csv_file_path = "test/multi_line.csv"

    File.write!(
      single_line_csv_file_path,
      " 2021/03/06,Dentist   \n",
      [
        :write,
        {:encoding, :utf8}
      ]
    )

    File.write!(
      multi_line_csv_file_path,
      " 2021/03/06,Dentist\n2021/03/07,Shopping\n2021/03/08,Movies\n",
      [
        :write,
        {:encoding, :utf8}
      ]
    )

    on_exit(fn ->
      File.rm!(single_line_csv_file_path)
      File.rm!(multi_line_csv_file_path)
    end)

    {:ok,
     single_line_csv_file_path: single_line_csv_file_path,
     multi_line_csv_file_path: multi_line_csv_file_path}
  end

  test "create a TodoList from imported from CSV file with single entry", state do
    single_line_csv_file_path = state[:single_line_csv_file_path]
    todo_list = CsvImporter.import(single_line_csv_file_path)

    assert todo_list == %TodoList{
             auto_id: 2,
             entries: %{
               1 => %TodoEntry{id: 1, date: ~D[2021-03-06], title: "Dentist"}
             }
           }
  end

  test "create a TodoList from imported from CSV file with many entry", state do
    multi_line_csv_file_path = state[:multi_line_csv_file_path]
    todo_list = CsvImporter.import(multi_line_csv_file_path)

    assert todo_list == %TodoList{
             auto_id: 4,
             entries: %{
               3 => %TodoEntry{id: 3, date: ~D[2021-03-08], title: "Movies"},
               2 => %TodoEntry{id: 2, date: ~D[2021-03-07], title: "Shopping"},
               1 => %TodoEntry{id: 1, date: ~D[2021-03-06], title: "Dentist"}
             }
           }
  end
end
