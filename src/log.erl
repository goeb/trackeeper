-module(log).

-compile(export_all).


info(Format, Args) -> error_logger:info_msg(Format, Args).
info(Format) -> error_logger:info_msg(Format).

