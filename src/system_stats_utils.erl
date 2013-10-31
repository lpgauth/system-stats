-module(system_stats_utils).
-include("system_stats.hrl").

-export([
    cpu_percent/2,
    new_stats/0,
    read_file/1,
    read_file/2,
    top/1
]).

-define(PAGE_SIZE, 4096).
-define(SLEEP, 500).

%% public
cpu_percent(#stats {
        cpu_cstime = Cstime,
        cpu_cutime = Cutime,
        cpu_stime = Stime,
        cpu_total = Total,
        cpu_utime = Utime
    }, #stats {
        cpu_cstime = Cstime2,
        cpu_cutime = Cutime2,
        cpu_stime = Stime2,
        cpu_total = Total2,
        cpu_utime = Utime2
    }) ->

    TotalDiff = Total2 - Total,
    Ucpu = 100 * (((Utime2 + Cutime2) - (Utime + Cutime)) / TotalDiff),
    Scpu = 100 * (((Stime2 + Cstime2) - (Stime + Cstime)) / TotalDiff),
    {Ucpu, Scpu}.

new_stats() -> #stats{}.

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
    {Ucpu, Scpu} = cpu_percent(Stats, Stats3),
    CpuPercent = trunc(CpuCores * (Ucpu + Scpu)),
    Vsize = bytes_to_megabytes(Stats3#stats.mem_vsize),
    Rss = bytes_to_megabytes(?PAGE_SIZE * (Stats3#stats.mem_rss)),
    io:format("cpu: ~p% vsize: ~pM rss: ~p~n", [CpuPercent, Vsize, Rss]),
    timer:sleep(?SLEEP),
    top_loop(Pid, Stats3).
