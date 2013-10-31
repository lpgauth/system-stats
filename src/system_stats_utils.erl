-module(system_stats_utils).
-include("system_stats.hrl").

-export([
    cpu_percent/2,
    read_file/1,
    read_file/2
]).

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

%% private
read(File, Acc) ->
    case file:read(File, 4096) of
        {ok, Data} when is_list(Data) ->
            read(File, Data ++ Acc);
        {ok, Data} when is_binary(Data) ->
            read(File, <<Data/binary, Acc/binary>>);
        {error, Reason} ->
            {error, Reason};
        eof ->
            {ok, Acc}
    end.