defmodule Chapter4.Todo.TodoList do
  @behaviour Access
  defstruct auto_id: 1, entries: %{}
  alias Chapter4.Todo.TodoEntry

  defimpl String.Chars, for: __MODULE__ do
    def to_string(_) do
      "#TodoList"
    end
  end

  defimpl Collectable, for: __MODULE__ do
    def into(original) do
      {original, &into_callback/2}
    end

    defp into_callback(todo_list, {:cont, entry}) do
      new_entry = TodoEntry.new(entry)
      Chapter4.Todo.TodoList.add_entry(todo_list, new_entry)
    end

    defp into_callback(todo_list, :done), do: todo_list
    defp into_callback(_todo_list, :halt), do: :ok
  end

  @doc """
    iex> Chapter4.Todo.TodoList.new()
    %Chapter4.Todo.TodoList{auto_id: 1, entries: %{}}
  """
  def new(), do: %__MODULE__{}

  def new(entries) do
    Enum.reduce(entries, new(), fn %{date: date, title: title}, todo_list_acc ->
      entry = %TodoEntry{date: date, title: title}
      add_entry(todo_list_acc, entry)
    end)
  end

  def add_entry(%__MODULE__{} = todo_list, %TodoEntry{} = entry) do
    entry = %TodoEntry{entry | id: todo_list.auto_id}
    path = [:entries, todo_list.auto_id]
    new_entries = put_in(todo_list, path, entry)

    %__MODULE__{new_entries | auto_id: todo_list.auto_id + 1}
  end

  def entries(todo_list, date) do
    todo_list.entries
    |> Stream.filter(fn {_, entry} -> entry.date == date end)
    |> Enum.map(fn {_, entry} -> entry end)
  end

  def update_entry(todo_list, %{} = new_entry) do
    update_entry(todo_list, new_entry.id, fn _ -> new_entry end)
  end

  def update_entry(todo_list, entry_id, updater_fun) do
    case Map.fetch(todo_list.entries, entry_id) do
      :error ->
        todo_list

      {:ok, old_entry} ->
        old_entry_id = old_entry.id
        new_entry = %{id: ^old_entry_id} = updater_fun.(old_entry)
        new_entries = Map.put(todo_list.entries, new_entry.id, new_entry)
        %__MODULE__{todo_list | entries: new_entries}
    end
  end

  def delete_entry(todo_list, entry_id) do
    new_entries = Map.delete(todo_list.entries, entry_id)
    %__MODULE__{todo_list | entries: new_entries}
  end

  def get_and_update(data, key, function) do
    Map.get_and_update(data, key, function)
  end
end

# defimpl Access, for: Chapter4.Todo.TodoList do
#  def get_and_update(data, key, fun) do
#  end
# end
