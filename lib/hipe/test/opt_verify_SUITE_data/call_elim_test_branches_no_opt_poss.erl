-module(call_elim_test_branches_no_opt_poss).

-export([test/1]).

test(A) ->
    if A > 0 ->
        false = has_a_field(#{b=>true}),
        false = has_a_field(#{b=>1, c=>2}),
        false = has_a_field(#{b=>3, c=>4}),
        ok;
       A =< 0 ->
        true = has_a_field(#{a=>q,     'A'=> nej}),
        true = has_a_field(#{a=>"hej", false=>true}),
        true = has_a_field(#{a=>3}),
        ok
    end,
    true  = has_b_field(#{a=>3, b=>"seven"}),
    false = has_b_field(#{"seventeen"=>17}),
    ok.

has_a_field(#{a:=_}) -> true;
has_a_field(#{}) -> false.

has_b_field(#{b:=_}) -> true;
has_b_field(#{}) -> false.