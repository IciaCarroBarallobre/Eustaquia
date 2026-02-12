%%% @doc EUnit tests for seesaw module (no I²C; only structure and specs).
%%% read/write require grisp_i2c and hardware, so we only check that the module
%%% loads and exports the expected functions.
-module(seesaw_tests).
-include_lib("eunit/include/eunit.hrl").

exports_read_test() ->
    ?assertEqual(true, erlang:function_exported(seesaw, read, 3)),
    ?assertEqual(true, erlang:function_exported(seesaw, read, 5)),
    ?assertEqual(true, erlang:function_exported(seesaw, read, 6)).

exports_write_test() ->
    ?assertEqual(true, erlang:function_exported(seesaw, write, 3)),
    ?assertEqual(true, erlang:function_exported(seesaw, write, 5)).
