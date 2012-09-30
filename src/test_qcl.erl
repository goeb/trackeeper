-module(test_qcl).


-export([do/0, match/2]).
-record(animal, {name,legs,noise}).
-include("/usr/lib/erlang/lib/stdlib-1.16.4/include/qlc.hrl").

match(Text, Pattern) ->
    case re:run(Text, Pattern) of
        {match, _} -> true;
        nomatch -> false
    end.

do() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(animal,
        [ {disc_copies, [node()]}, 
          {attributes, record_info(fields,animal)}]),
    Fun = fun() -> mnesia:write(#animal{name="cow",legs=4,noise="moo"}) end,
    mnesia:transaction(Fun),
    Fun2 = fun() -> mnesia:write(#animal{name="sheep",legs=4,noise="bee"}) end,
    mnesia:transaction(Fun2),
    mnesia:transaction(fun() -> qlc:eval(qlc:q([X || X <- mnesia:table(animal)])) end).

