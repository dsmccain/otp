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

call_elim() ->
    [{doc, "Test that the call elimination optimization pass is ok"}].
call_elim(Config) ->
    F = filename:join(?config(data_dir, Config), "call_elim_test.erl"),
    FileNameElim = test_server:temp_name(filename:join(?config(priv_dir, Config), "call_elim_out")),
    {ok, TestCase} = compile:file(F),
    {ok, TestCase} = hipe:c(TestCase, [icode_call_elim, {pp_range_icode, {file, FileNameElim}}]),
    {ok, ElimFile} = file:read_file(FileNameElim),
    file:delete(FileNameElim),
    ok = TestCase:test(),
    FileNameNoElim = test_server:temp_name(filename:join(?config(priv_dir, Config), "no_call_elim_out")),
    {ok, TestCase} = hipe:c(TestCase, [no_icode_call_elim, {pp_range_icode, {file, FileNameNoElim}}]),
    {ok, NoElimFile} = file:read_file(FileNameNoElim),
    file:delete(FileNameNoElim),
    0 = string:str(binary:bin_to_list(ElimFile), "is_key"),
    true = (0 /= string:str(binary:bin_to_list(NoElimFile), "is_key")),
    ok.
