defmodule Chapter4.TodoList.TodoEntry do
  @moduledoc false
  defstruct id: nil, date: nil, title: nil

  def new(%{date: date, title: title}) do
    %__MODULE__{date: date, title: title}
  end
end
