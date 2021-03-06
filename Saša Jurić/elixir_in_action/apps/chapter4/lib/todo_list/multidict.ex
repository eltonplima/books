defmodule Chapter4.TodoList.MultiDict do
  @doc """
    iex> Chapter4.TodoList.MultiDict.new()
    %{}
  """
  def new(), do: %{}

  @doc """
    iex> multi_dict = Chapter4.TodoList.MultiDict.new()
    iex> multi_dict = Chapter4.TodoList.MultiDict.add(multi_dict, "key1", "value1")
    %{"key1" => ["value1"]}
    iex> Chapter4.TodoList.MultiDict.add(multi_dict, "key1", "value2")
    %{"key1" => ["value2", "value1"]}
  """
  def add(dict, key, value) do
    Map.update(dict, key, [value], &[value | &1])
  end

  @doc """
    iex> multi_dict = Chapter4.TodoList.MultiDict.new()
    iex> multi_dict = Chapter4.TodoList.MultiDict.add(multi_dict, "key1", "value1")
    iex> multi_dict = Chapter4.TodoList.MultiDict.add(multi_dict, "key1", "value2")
    iex> Chapter4.TodoList.MultiDict.get(multi_dict, "key1")
    ["value2", "value1"]
    iex> Chapter4.TodoList.MultiDict.get(multi_dict, "key2")
    []
  """
  def get(dict, key) do
    Map.get(dict, key, [])
  end
end
