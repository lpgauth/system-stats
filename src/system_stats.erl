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
proc_cpuinfo(#stats {} = Stats) ->
    {ok, CpuInfo} = system_stats_utils:read_file("/proc/cpuinfo", [binary]),
    CpuCores = length(binary:split(CpuInfo, <<"processor\t:">>, [global, trim])) - 1,
    Stats#stats {
       cpu_cores = CpuCores
    }.

proc_pid_stat(#stats {} = Stats) ->
    proc_pid_stat(os:getpid(), Stats).

proc_pid_stat(Pid, Stats) when is_integer(Pid) ->
    proc_pid_stat(integer_to_list(Pid), Stats);
proc_pid_stat(Pid, #stats {} = Stats) ->
    Filename = "/proc/" ++ Pid ++ "/stat",
    {ok, ProcStat} = system_stats_utils:read_file(Filename),
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
    {ok, LoadAvg} = system_stats_utils:read_file("/proc/loadavg"),
    {ok, [Load1, Load5, Load15], _} = io_lib:fread("~f ~f ~f", LoadAvg),
    Stats#stats {
        load_1 = Load1,
        load_5 = Load5,
        load_15 = Load15
    }.

proc_stat(#stats {} = Stats) ->
    {ok, ProcStat} = system_stats_utils:read_file("/proc/stat"),
    {ok, Times, _} = io_lib:fread("cpu ~u ~u ~u ~u ~u ~u ~u ~u ~u ~u", ProcStat),
    CpuTotal = lists:foldl(fun(X, Sum) -> X + Sum end, 0, Times),
    Stats#stats {
        cpu_total = CpuTotal
    }.
