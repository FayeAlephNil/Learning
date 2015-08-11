defmodule LearningElixirTest do
  use ExUnit.Case

  test "the truth" do
    assert 1 + 1 == 2
  end

  test "factorial" do
    assert LearningElixir.factorial(0) == 1
    assert LearningElixir.factorial(5) == 120
  end
end
