-module(tke_user).

-compile(export_all). % TODO

%% Return name of user that is logged in
get_author() -> "John Doo".

get_list_of_users() -> ["John Doo", "Alice Cain", "Emerald Lake"].

