defmodule PollutionData do
  @moduledoc false

  def  importLinesFromCSV(filename) do
    lines = File.read!(filename)
            |> String.split("\r\n")
    IO.puts("#{length(lines)} lines has been read")
    lines
  end

  def convertLine(line) do
    [date, time, length, width, value] = String.split(line, ",")

    date = String.split(date, "-")
           |> Enum.reverse
           |> Enum.map(fn x -> {val, _} = Integer.parse(x); val end)
           |> :erlang.list_to_tuple()

    hour = String.split(hour, ":")
           |> Enum.map(fn x -> {val, _} = Integer.parse(x); val end)
    hour = hour ++ [0]
           |> :erlang.list_to_tuple()

    coords = [width, length]
             |> Enum.map(fn x -> {val, _} = Float.parse(x); val end)
             |> :erlang.list_to_tuple()

    {value, _} = Integer.parse(value)

    %{:datetime => {date, hour}, :location => coords, :pollutionLevel => value}
  end

end
