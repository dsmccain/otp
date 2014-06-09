-module(call_elim_test).

-export([test/0]).

test() ->
    true = has_a_field(#{a=>true}),
    true = has_a_field(#{a=>1, b=>2}),
    true = has_a_field(#{b=>3, a=>4}),
    ok.

has_a_field(#{a:=_}) -> true;
has_a_field(#{}) -> false.
