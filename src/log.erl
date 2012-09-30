-module(log).

-compile(export_all).


info(Format, Args) -> error_logger:info_msg("INFO " ++ Format ++ "\n", Args).
info(Format) -> info(Format, []).

debug(Format) -> error_logger:info_msg(Format, []).
debug(Format, Args) -> error_logger:info_msg("DEBUG: " ++ Format ++ "\n", Args).

