using CSV, DataFrames#, Statistics, Printf

MATCH_VERSION = r"^Affinity (?P<v>.+)"
MATCH_THREAD = r"^Threads: (?P<num>[0-9]+)"
MATCH_TIME = r"^.*(?P<loop>[0-9]) =\s+(?P<time>[0-9]+\.[0-9]+)"

function clean()

  open("data/raw.txt") do f
    cur_version = nothing
    cur_threads = nothing
    d = Dict()

    for l in eachline(f)

      m = match(MATCH_VERSION, l)
      if m !== nothing
        cur_version = m[:v]
        d[cur_version] = Dict()
      end

      m = match(MATCH_THREAD, l)
      if m !== nothing
        cur_threads = parse(Int64, m[:num])
        d[cur_version][cur_threads] = [[.0, .0], [.0, .0]]
      end

      m = match(MATCH_TIME, l)
      if m !== nothing
        loop = parse(Int64, m[:loop])
        time = parse(Float64, m[:time])
        d[cur_version][cur_threads][loop] += [1., time]
      end
    end

    println("version,threads,loop,avg_time")
    for v in keys(d)
      for t in sort(collect(keys(d[v])))
        for (l, x) in enumerate(d[v][t])
          println(v,",",t,",",l,",",x[2] / x[1])
        end
      end
    end
  end
end


function speedup_data(version, loop)
  data = CSV.read("data/clean.csv", delim=',', copycols=true)

  p1m = nothing

  for amnt_tds in groupby(data, [:version, :loop, :threads])
    if amnt_tds[!, :version][1] == version && amnt_tds[!, :loop][1] == loop
      if amnt_tds[!, :threads][1] == 1
        p1m = amnt_tds[!, :avg_time][1]
      end
      println(amnt_tds[!, :threads][1], ",", p1m / amnt_tds[!, :avg_time][1])
    end
  end
end


function time_data(version, loop)
  data = CSV.read("data/clean.csv", delim=',', copycols=true)

  for amnt_tds in groupby(data, [:version, :loop, :threads])
    if amnt_tds[!, :version][1] == version && amnt_tds[!, :loop][1] == loop
      println(amnt_tds[!, :threads][1], ",", amnt_tds[!, :avg_time][1])
    end
  end
end


#speedup_data("Naive", 1)
#speedup_data("Naive", 2)
#speedup_data("Queue", 1)
#speedup_data("Queue", 2)

#time_data("Naive", 1)
#time_data("Naive", 2)
#time_data("Queue", 1)
time_data("Queue", 2)

