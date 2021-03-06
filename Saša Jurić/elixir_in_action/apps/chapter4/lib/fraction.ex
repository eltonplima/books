defmodule Fraction do
  @moduledoc false
  defstruct a: nil, b: nil

  def new(a, b) do
    %__MODULE__{a: a, b: b}
  end

  def value(%__MODULE__{a: a, b: b}) do
    a / b
  end

  def add(%__MODULE__{a: a1, b: b1}, %__MODULE__{a: a2, b: b2}) do
    new(a1 * b2 + a2 * b1, b2 * b1)
  end
end
