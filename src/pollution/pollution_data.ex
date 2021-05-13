defmodule PollutionData do
  def  importLinesFromCSV(filename) do
    lines = File.read!(filename)
            |> String.split("\r\n")
    lines
  end

  def convertLine(line) do
    [date, time, length, width, value] = String.split(line, ",")

    date = String.split(date, "-")
           |> Enum.reverse
           |> Enum.map(fn x -> {val, _} = Integer.parse(x); val end)
           |> :erlang.list_to_tuple()

    hour = String.split(time, ":")
           |> Enum.map(fn x -> {val, _} = Integer.parse(x); val end)
    hour = hour ++ [0]
           |> :erlang.list_to_tuple()

    coords = [length, width]
             |> Enum.map(fn x -> {val, _} = Float.parse(x); val end)
             |> :erlang.list_to_tuple()

    {value, _} = Integer.parse(value)

    %{:datetime => {date, hour}, :location => coords, :pollutionLevel => value}
  end

  def convertLines(lines) do
    data = lines
           |> Enum.map(fn line -> PollutionData.convertLine(line) end)
    data
  end

  def identifyStations(data) do
    stations = data
               |> Enum.map(fn x -> x.location end)
               |> Enum.uniq()
    stations
  end

  def loadStations(stations) do
    :pollution_gen_server.start_link()
    stations
    |> Enum.each(
         fn {x, y} ->
           "station_#{x}_#{y}"
           |> :pollution_gen_server.addStation({x, y})
         end
       )
  end

  def loadStationsData(data) do
    data
    |> Enum.each(fn x -> :pollution_gen_server.addValue(x.location, x.datetime, "PM10", x.pollutionLevel) end)
  end

  def measureTime(fun) do
    fun
    |> :timer.tc
    |> elem(0)
    |> Kernel./(1_000_000)
  end

  def getStationMean() do
    :pollution_gen_server.getStationMean({20.06, 49.986}, "PM10")
  end

  def test(filename) do
    data = filename
           |> importLinesFromCSV()
           |> Enum.map(&convertLine/1)

    stations = identifyStations(data)

    IO.puts("-----------TEST-----------")
    IO.puts("loadStations time: #{measureTime(fn -> loadStations(stations) end)}")
    IO.puts("loadStationsData time: #{measureTime(fn -> loadStationsData(data) end)}")
    IO.puts(
      "getStationMean time: #{measureTime(fn -> :pollution_gen_server.getStationMean({20.06, 49.986}, "PM10") end)}"
    )
    IO.puts(
      "getStationMean time: #{
        measureTime(fn -> :pollution_gen_server.getDailyMean("PM10", {{2017, 5, 3}, {0, 0, 0}}) end)
      }"
    )
  end

end