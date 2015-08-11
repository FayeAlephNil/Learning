defmodule MathsTest do
  use ExUnit.Case

  test "factorial" do
    assert Maths.fac(0) == 1
    assert Maths.fac(5) == 120
  end
end
