-module(system_stats_utils).
-include("system_stats.hrl").

-export([
    cpu_percent/2,
    read_file/1
]).

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


read_file(Filename) ->
    case file:open(Filename, [read, raw]) of
        {ok, File} ->
            Return = read(File, []),
            ok = file:close(File),
            Return;
        {error, Reason} ->
            {error, Reason}
    end.

read(File, Acc) ->
    case file:read(File, 4096) of
        {ok, Data} ->
            read(File, [Data | Acc]);
        {error, Reason} ->
            {error, Reason};
        eof ->
            [Data] = Acc,
            {ok, Data}
    end.