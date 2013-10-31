-module(system_stats).
-include("system_stats.hrl").

-export([
    proc_cpuinfo/1,
    proc_load_avg/1,
    proc_pid_stat/1,
    proc_pid_stat/2,
    proc_stat/1
]).

%% public
proc_cpuinfo(_) -> % #stats {} = _Stats) ->
    {ok, CpuInfo} = system_stats_utils:read_file("/proc/cpuinfo"),
    io:format("~p~n", [CpuInfo]).

proc_pid_stat(#stats {} = Stats) ->
    proc_pid_stat(os:getpid(), Stats).

proc_pid_stat(Pid, #stats {} = Stats) ->
    Filename = "/proc/" ++ Pid ++ "/stat",
    {ok, File} = file:open(Filename, [read, raw]),
    {ok, ProcStat} = file:read(File, 400),
    ok = file:close(File),
    {ok, [_, _, _, _, _, _, _, _, _, _, _, _, _, Utime, Stime, Cutime, Cstime,
        _, _, _, _, _, Vsize, Rss], _} = io_lib:fread("~d ~s ~c ~d ~d ~d ~d
        ~d ~u ~u ~u ~u ~u ~u ~u ~d ~d ~d ~d ~d ~d ~u ~u ~d", ProcStat),
    Stats#stats {
        cpu_cstime = Cstime,
        cpu_cutime = Cutime,
        cpu_stime = Stime,
        cpu_utime = Utime,
        mem_rss = Rss,
        mem_vsize = Vsize
    }.

proc_load_avg(#stats {} = Stats) ->
    {ok, File} = file:open("/proc/loadavg", [read, raw]),
    {ok, LoadAvg} = file:read(File, 40),
    ok = file:close(File),
    {ok, [Load1, Load5, Load15], _} = io_lib:fread("~f ~f ~f", LoadAvg),
    Stats#stats {
        load_1 = Load1,
        load_5 = Load5,
        load_15 = Load15
    }.

proc_stat(#stats {} = Stats) ->
    {ok, File} = file:open("/proc/stat", [read, raw]),
    {ok, ProcStat} = file:read(File, 100),
    ok = file:close(File),
    {ok, Times, _} = io_lib:fread("cpu ~u ~u ~u ~u ~u ~u ~u ~u ~u ~u", ProcStat),
    CpuTotal = lists:foldl(fun(X, Sum) -> X + Sum end, 0, Times),
    Stats#stats {
        cpu_total = CpuTotal
    }.
