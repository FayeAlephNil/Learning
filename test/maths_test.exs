defmodule MathsTest do
  use ExUnit.case

  test "factorial" do
    assert Maths.factorial(0) == 1
    assert Maths.factorial(5) == 120
  end
end
