-module(opt_verify_SUITE).

-include_lib("test_server/include/test_server.hrl").

-compile([export_all]).

all() ->
    [call_elim].

groups() ->
    [].

init_per_suite(Config) ->
    case erlang:system_info(hipe_architecture) of
        undefined -> {skip, "HiPE not available or enabled"};
        _ -> Config
    end.

end_per_suite(_Config) ->
     ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

call_elim_test_file(Config, FileName, Option) ->
    TempOut = test_server:temp_name(filename:join(?config(priv_dir, Config), "call_elim_out")),
    {ok, TestCase} = compile:file(FileName),
    {ok, TestCase} = hipe:c(TestCase, [Option, {pp_range_icode, {file, TempOut}}]),
    {ok, Icode} = file:read_file(TempOut),
    file:delete(TempOut),
    Icode.

number_of_substring(Icode, Substring) ->
    number_of_substring(Icode, Substring, 0).
number_of_substring(Icode, Substring, N) ->
    case string:str(Icode, Substring) of
        0 ->
            N;
        I ->
            number_of_substring(lists:nthtail(I, Icode), Substring, N+1)
    end.

call_elim() ->
    [{doc, "Test that the call elimination optimization pass is ok"}].
call_elim(Config) ->
    F1 = filename:join(?config(data_dir, Config), "call_elim_test.erl"),
    Icode1 = call_elim_test_file(Config, F1, icode_call_elim),
    0 = number_of_substring(binary:bin_to_list(Icode1), "is_key"),
    Icode2 = call_elim_test_file(Config, F1, no_icode_call_elim),
    true = (0 /= number_of_substring(binary:bin_to_list(Icode2), "is_key")),
    F2 = filename:join(?config(data_dir, Config), "call_elim_test_branches_no_opt_poss.erl"),
    Icode3 = call_elim_test_file(Config, F2, icode_call_elim),
    3 = number_of_substring(binary:bin_to_list(Icode3), "is_key"),
    Icode4 = call_elim_test_file(Config, F2, no_icode_call_elim),
    3 = number_of_substring(binary:bin_to_list(Icode4), "is_key"),
    F3 = filename:join(?config(data_dir, Config), "call_elim_test_branches_opt_poss.erl"),
    Icode5 = call_elim_test_file(Config, F3, icode_call_elim),
    0 = number_of_substring(binary:bin_to_list(Icode5), "is_key"),
    Icode6 = call_elim_test_file(Config, F3, no_icode_call_elim),
    3 = number_of_substring(binary:bin_to_list(Icode6), "is_key"),
    ok.
