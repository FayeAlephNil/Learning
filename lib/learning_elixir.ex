defmodule LearningElixir do
  def factorial(0) do
    1
  end

  def factorial(x) when is_number(x) and x > 0 do
    x * factorial(x-1)
  end
end
