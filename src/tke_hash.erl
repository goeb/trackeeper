%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Compute hash
%%%

-module(tke_hash).
-export([create_hash/1]).

create_hash(Contents) ->
    Bin = term_to_binary(Contents),
    Ref = term_to_binary(make_ref()),
    Concat = << Bin/bytes, Ref/bytes>>,
    Sha = crypto:sha(Concat),
    base34(Sha).

%% Return a string representation using characters 0-9, A-Z minus 'I' ad 'O'
base34(Binary) ->
    Length = size(Binary)*8, % number of bits in binary
    <<Value:Length>> = Binary, % convert to integer
    Hash = integer_to_list(Value, 34), % characters used: 0-9, A-X
    [ replace_I_O(Char) || Char <- Hash ].


%% replace 'I' by 'Y' and 'O' by 'Z'
replace_I_O($I) -> $Y;
replace_I_O($O) -> $Z;
replace_I_O(C) -> C.



