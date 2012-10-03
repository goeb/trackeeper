-module(log).

-compile(export_all).

get_timestamp() ->
    Format = "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
    {{Y, Month, D}, {H, Min, S}} = calendar:local_time(),
    io_lib:format(Format, [Y, Month, D, H, Min, S]).

info(Format, Args) -> log("INFO", Format, Args).
info(Format) -> info(Format, []).

log(Level, Format, Args) ->
    io:format(get_timestamp() ++ " " ++ Level ++ " " ++ Format ++ "\n", Args).

debug(Format) -> debug(Format, []).
debug(Format, Args) -> log("DEBUG", Format, Args).

