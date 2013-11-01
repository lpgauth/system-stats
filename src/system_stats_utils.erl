-module(system_stats_utils).
-include("system_stats.hrl").

-export([
    cpu_percent/2,
    new_stats/0,
    supported_os/0,
    read_file/1,
    read_file/2,
    top/1
]).

-define(PAGE_SIZE, 4096).
-define(SLEEP, 500).

%% public
cpu_percent(#stats {
        cpu_cstime = CpuCstime,
        cpu_cutime = CpuCutime,
        cpu_stime = CpuStime,
        cpu_total = CpuTotal,
        cpu_utime = CpuUtime
    }, #stats {
        cpu_cstime = CpuCstime2,
        cpu_cutime = CpuCutime2,
        cpu_stime = CpuStime2,
        cpu_total = CpuTotal2,
        cpu_utime = CpuUtime2
    }) ->

    CpuTotalDiff = CpuTotal2 - CpuTotal,
    CpuUser = 100 * (((CpuUtime2 + CpuCutime2) -
        (CpuUtime + CpuCutime)) / CpuTotalDiff),
    CpuSystem = 100 * (((CpuStime2 + CpuCstime2) -
        (CpuStime + CpuCstime)) / CpuTotalDiff),
    {CpuUser, CpuSystem}.

new_stats() -> #stats{}.

supported_os() ->
    case os:type() of
        {_, Linux} -> Linux;
        _Else -> undefined
    end.

read_file(Filename) ->
    read_file(Filename, []).

read_file(Filename, Options) ->
    DefaultOptions = [read, raw],
    case file:open(Filename, DefaultOptions ++ Options) of
        {ok, File} ->
            Return =
                case lists:any(fun (Opt) -> Opt =:= binary end, Options) of
                    true ->
                        read(File, <<>>);
                    false ->
                        read(File, "")
                end,
            ok = file:close(File),
            Return;
        {error, Reason} ->
            {error, Reason}
    end.

top(Pid) ->
    Stats = system_stats:proc_cpuinfo(new_stats()),
    Stats2 = system_stats:proc_stat(Stats),
    Stats3 = system_stats:proc_pid_stat(Pid, Stats2),
    timer:sleep(?SLEEP),
    top_loop(Pid, Stats3).

%% private
bytes_to_megabytes(Bytes) ->
    Bytes / 1048576.

kilobytes_to_megabytes(Kilobytes) ->
    Kilobytes / 1024.

read(File, Acc) ->
    case file:read(File, 4096) of
        {ok, Data} when is_list(Data) ->
            read(File, [Data | Acc]);
        {ok, Data} when is_binary(Data) ->
            read(File, <<Data/binary, Acc/binary>>);
        {error, Reason} ->
            {error, Reason};
        eof when is_list(Acc) ->
            {ok, lists:flatten(lists:reverse(Acc))};
        eof when is_binary(Acc) ->
            {ok, Acc}
    end.

top_loop(Pid, #stats {cpu_cores = CpuCores} = Stats) ->
    Stats2 = system_stats:proc_stat(Stats),
    Stats3 = system_stats:proc_pid_stat(Pid, Stats2),
    Stats4 = system_stats:proc_meminfo(Stats3),
    {Ucpu, Scpu} = cpu_percent(Stats, Stats4),
    Vsize = bytes_to_megabytes(Stats4#stats.mem_vsize),
    Rss = bytes_to_megabytes(?PAGE_SIZE * (Stats4#stats.mem_rss)),
    CpuPercent = (CpuCores * (Ucpu + Scpu)),
    MemPercent = 100 * (Rss / kilobytes_to_megabytes(Stats4#stats.mem_total)),
    io:format("vsize: ~pM rss: ~pM cpu: ~p% mem: ~p%~n", [trunc(Vsize),
        trunc(Rss), trunc(CpuPercent), trunc(MemPercent)]),
    timer:sleep(?SLEEP),
    top_loop(Pid, Stats3).
