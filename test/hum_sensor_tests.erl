%%% @doc EUnit tests for hum_sensor (pure functions and behaviour).
%%% I²C-dependent functions (get_moisture, read_temperature) are not tested here;
%%% they require GRiSP hardware.
-module(hum_sensor_tests).
-include_lib("eunit/include/eunit.hrl").

%%% raw_to_celsius/1: seesaw STATUS_TEMP is 32-bit signed fixed-point, °C = raw/65536.
%%% For raw >= 16#80000000 we treat as negative (two's complement).

raw_to_celsius_zero_test() ->
    ?assertEqual(0.0, hum_sensor:raw_to_celsius(0)).

raw_to_celsius_positive_test() ->
    %% 22.5 °C ≈ 22.5 * 65536 = 1474560
    Raw = round(22.5 * 65536),
    ?assertEqual(22.5, hum_sensor:raw_to_celsius(Raw)).

raw_to_celsius_positive_simple_test() ->
    %% 65536 raw = 1.0 °C
    ?assertEqual(1.0, hum_sensor:raw_to_celsius(65536)).

raw_to_celsius_negative_test() ->
    %% -10 °C: signed 32-bit fixed-point → raw = 4294967296 - 655360 = 4294311936
    ?assertEqual(-10.0, hum_sensor:raw_to_celsius(4294311936)).

raw_to_celsius_negative_exact_test() ->
    %% raw = 16#80000000 is -2147483648 in 32-bit signed → -32768.0 in fixed-point
    ?assertEqual(-32768.0, hum_sensor:raw_to_celsius(16#80000000)).
