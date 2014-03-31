-module(maps_expand_map_update).
-export([test/0]).

test() ->
    {skip, "Not yet supported by HiPE."};
test() ->
    M = #{<<"hello">> => <<"world">>}#{<<"hello">> := <<"les gens">>},
    #{<<"hello">> := <<"les gens">>} = M,
    ok.
