defmodule Maths do
  def fac(0), do: 1

  def fac(n) when n > 0, do: Enum.reduce(1..n, 1, &*/2) 
end
