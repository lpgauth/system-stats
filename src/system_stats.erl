-module(system_stats).
-include("system_stats.hrl").

-export([
    proc_cpuinfo/1,
    proc_loadavg/1,
    proc_meminfo/1,
    proc_pidstat/1,
    proc_pidstat/2,
    proc_stat/1,
    supported_os/0
]).

%% public
proc_cpuinfo(#stats {} = Stats) ->
    proc_cpuinfo(supported_os(), #stats {} = Stats).

proc_meminfo(#stats {} = Stats) ->
    proc_meminfo(supported_os(), #stats {} = Stats).

proc_loadavg(#stats {} = Stats) ->
    proc_loadavg(supported_os(), #stats {} = Stats).

proc_pidstat(#stats {} = Stats) ->
    proc_pidstat(supported_os(), os:getpid(), Stats).

proc_pidstat(Pid, #stats {} = Stats) when is_integer(Pid) ->
    proc_pidstat(supported_os(), integer_to_list(Pid), Stats);
proc_pidstat(Pid, #stats {} = Stats) when is_list(Pid) ->
    proc_pidstat(supported_os(), Pid, Stats).

proc_stat(#stats {} = Stats) ->
    proc_stat(supported_os(), #stats {} = Stats).

%% private
proc_cpuinfo(linux, Stats) ->
    {ok, CpuInfo} = system_stats_utils:read_file("/proc/cpuinfo", [binary]),
    CpuCores = length(binary:split(CpuInfo, <<"processor\t:">>, [global, trim])) - 1,

    Stats#stats {
       cpu_cores = CpuCores
    };
proc_cpuinfo(undefined, Stats) ->
    Stats.

proc_meminfo(linux, Stats) ->
    {ok, MemInfo} = system_stats_utils:read_file("/proc/meminfo"),
    {ok, [_, MemTotal, "kB"], _} = io_lib:fread("~s ~u ~s", MemInfo),

    Stats#stats {
        mem_total = MemTotal
    };
proc_meminfo(undefined, Stats) ->
    Stats.

proc_loadavg(linux, Stats) ->
    {ok, LoadAvg} = system_stats_utils:read_file("/proc/loadavg"),
    {ok, [Load1, Load5, Load15], _} = io_lib:fread("~f ~f ~f", LoadAvg),

    Stats#stats {
        load_1 = Load1,
        load_5 = Load5,
        load_15 = Load15
    };
proc_loadavg(undefined, Stats) ->
    Stats.

proc_pidstat(linux, Pid, Stats) ->
    Filename = "/proc/" ++ Pid ++ "/stat",
    {ok, ProcStat} = system_stats_utils:read_file(Filename),
    {ok, [_, _, _, _, _, _, _, _, _, _, _, _, _, CpuUtime, CpuStime, CpuCutime,
        CpuCstime, _, _, _, _, _, MemVsize, MemRss], _} = io_lib:fread("~d ~s
        ~c ~d ~d ~d ~d ~d ~u ~u ~u ~u ~u ~u ~u ~d ~d ~d ~d ~d ~d ~u ~u ~d",
        ProcStat),

    Stats#stats {
        cpu_cstime = CpuCstime,
        cpu_cutime = CpuCutime,
        cpu_stime = CpuStime,
        cpu_utime = CpuUtime,
        mem_rss = MemRss,
        mem_vsize = MemVsize
    };
proc_pidstat(undefined, _Pid, Stats) ->
    Stats.

proc_stat(linux, Stats) ->
    {ok, ProcStat} = system_stats_utils:read_file("/proc/stat"),
    {ok, Times, _} = io_lib:fread("cpu ~u ~u ~u ~u ~u ~u ~u ~u ~u ~u", ProcStat),
    CpuTotal = lists:foldl(fun(X, Sum) -> X + Sum end, 0, Times),

    Stats#stats {
        cpu_total = CpuTotal
    };
proc_stat(undefined, Stats) ->
    Stats.

supported_os() ->
    case os:type() of
        {_, linux} -> linux;
        _Else -> undefined
    end.
