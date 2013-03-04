-module(log).

-compile(export_all).

get_timestamp() ->
    Format = "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
    {{Y, Month, D}, {H, Min, S}} = calendar:local_time(),
    io_lib:format(Format, [Y, Month, D, H, Min, S]).

error(Format, Args) -> log("ERROR", Format, Args).
error(Format) -> error(Format, []).

info(Format, Args) -> log("INFO", Format, Args).
info(Format) -> info(Format, []).

log(Level, Format, Args) ->
    Pid = io_lib:format("~p", [self()]),
    io:format(get_timestamp() ++ " " ++ Pid ++ " " ++ Level ++ " "
        ++ Format ++ "\n", Args).

debug(Format) -> debug(Format, []).
debug(Format, Args) -> log("DEBUG", Format, Args).

