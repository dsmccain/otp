-module(maps_put_map_exact).
-export([test/0]).

test() ->
    false = exact_guard(#{b=>a}),
    false = exact_guard(not_a_map),
    true  = exact_guard(#{a=>false}),
    #{a:=true} = exact_update(#{a=>false}),
    {'EXIT', {badarg, [{maps_put_map_exact, exact_update, 1, _}|_]}}
	= (catch exact_update(not_a_map)),
    {'EXIT', {badarg, [{maps_put_map_exact, exact_update, 1, _}|_]}}
	= (catch exact_update(#{})),
    ok.

exact_guard(M) when is_map(M#{a:=b}) -> true;
exact_guard(_) -> false.

exact_update(M) -> M#{a:=true}.
