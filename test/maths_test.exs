defmodule MathsTest do
  use ExUnit.Case, async: true

  test "factorial" do
    assert Maths.fac(0) == 1
    assert Maths.fac(5) == 120
  end
end
