%%% @doc EUnit tests for eustaquia application (module load and API).
%%% No hardware is used; only checks that the application and modules are loadable
%%% and that the public API is present.
-module(eustaquia_tests).
-include_lib("eunit/include/eunit.hrl").

application_load_test() ->
    ?assertEqual(ok, application:load(eustaquia)).

exports_loop_test() ->
    ?assertEqual(true, erlang:function_exported(eustaquia, loop, 0)).

exports_check_sensor_test() ->
    ?assertEqual(true, erlang:function_exported(eustaquia, check_sensor, 0)).

exports_test_moisture_test() ->
    ?assertEqual(true, erlang:function_exported(eustaquia, test_moisture, 0)).

exports_test_servo_test() ->
    ?assertEqual(true, erlang:function_exported(eustaquia, test_servo_happy, 0)),
    ?assertEqual(true, erlang:function_exported(eustaquia, test_servo_sad, 0)).
