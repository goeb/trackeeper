#!/usr/bin/env escript


main(Args) -> 
    %edoc:files(["src/tke_db.erl"], []).
    edoc:files(Args, []).
