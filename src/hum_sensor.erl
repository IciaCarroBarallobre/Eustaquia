-module(hum_sensor).

-moduledoc """
Module for reading soil moisture and temperature from the Adafruit STEMMA
Soil Sensor (I2C capacitive moisture sensor, JST PH 2mm) via I2C.

Uses the seesaw protocol (ATSAMD10 on the sensor) via the `seesaw` module,
aligned with [Adafruit_Seesaw Arduino](https://github.com/adafruit/Adafruit_Seesaw)
and CircuitPython: delay before read for conversion time, retries for touch/moisture.
No Arduino or Python required; GRiSP I2C PMOD (`grisp_i2c`).

- **Moisture**: capacitive reading, typically ~200 (very dry) to ~2000 (very wet).
- **Temperature**: onboard chip temperature, approx. ±2 °C.

### Example usage:

```erlang
1> hum_sensor:read_moisture().
Moisture: 850
{ok, 850}

2> hum_sensor:read_temperature().
{ok, 22.5}
```
""".

-export([read_moisture/0, read_temperature/0, get_moisture/0, raw_to_celsius/1]).

%% Seesaw register map (firmware: github.com/adafruit/seesaw include/RegisterMap.h)
-define(TOUCH_BASE, 16#0F).          %% SEESAW_TOUCH_BASE
-define(TOUCH_CH0, 16#10).           %% SEESAW_TOUCH_CHANNEL_0
-define(STATUS_BASE, 16#00).         %% SEESAW_STATUS_BASE
-define(STATUS_TEMP, 16#04).         %% SEESAW_STATUS_TEMP

%% Arduino Adafruit_Seesaw: touchRead uses delay 3000+retry*1000 ms, getTemp 1000 ms
-define(TOUCH_DELAY_MS, 10).         %% delay before read (Arduino uses 3000+; we use 10)
-define(TOUCH_RETRIES, 5).           %% max retries like Arduino touchRead
-define(TEMP_DELAY_MS, 100).         %% delay before read (Arduino 1000 ms; we use 100)
-define(DEFAULT_BUS, i2c1).
-define(DEFAULT_ADDR, 16#36).

-doc """
Reads capacitive soil moisture from the Adafruit STEMMA Soil Sensor.

Uses seesaw touch module (base 0x0F, channel 0). Typical range:
~200 (very dry) to ~2000 (very wet); actual range depends on soil.

Returns:
- {ok, Moisture}: The capacitive reading (0-65535)
- {error, Reason}: If I2C communication fails.
""".
-spec read_moisture() -> {ok, non_neg_integer()} | {error, term()}.
read_moisture() ->
    case get_moisture() of
        {ok, M} ->
            io:format("Moisture: ~p~n", [M]),
            {ok, M};
        E ->
            io:format("Error reading moisture: ~p~n", [E]),
            E
    end.

-doc """
Same as read_moisture/0 but does not print (for use in check_sensor or loop).
Uses delay before read and retries like Arduino touchRead().
""".
-spec get_moisture() -> {ok, non_neg_integer()} | {error, term()}.
get_moisture() ->
    get_moisture_retry(0).

get_moisture_retry(N) when N < ?TOUCH_RETRIES ->
    case seesaw:read(?DEFAULT_BUS, ?DEFAULT_ADDR, ?TOUCH_BASE, ?TOUCH_CH0, 2,
                    ?TOUCH_DELAY_MS) of
        {ok, <<Hi:8, Lo:8>>} ->
            {ok, (Hi bsl 8) bor Lo};
        _Error ->
            get_moisture_retry(N + 1)
    end;
get_moisture_retry(_N) ->
    case seesaw:read(?DEFAULT_BUS, ?DEFAULT_ADDR, ?TOUCH_BASE, ?TOUCH_CH0, 2,
                    ?TOUCH_DELAY_MS) of
        {ok, <<Hi:8, Lo:8>>} ->
            {ok, (Hi bsl 8) bor Lo};
        Other ->
            {error, Other}
    end.

-doc """
Reads ambient temperature from the sensor’s onboard chip (seesaw STATUS_TEMP).

Accuracy is approximately ±2 °C. Uses seesaw status module (base 0x00, reg 0x04);
value is 4-byte big-endian signed fixed-point, °C = raw / 65536.

Returns:
- {ok, Celsius}: Temperature in degrees Celsius (float)
- {error, Reason}: If I2C communication fails.
""".
-spec read_temperature() -> {ok, float()} | {error, term()}.
read_temperature() ->
    case seesaw:read(?DEFAULT_BUS, ?DEFAULT_ADDR, ?STATUS_BASE, ?STATUS_TEMP, 4,
                    ?TEMP_DELAY_MS) of
        {ok, <<B0:8, B1:8, B2:8, B3:8>>} ->
            Raw = (B0 bsl 24) bor (B1 bsl 16) bor (B2 bsl 8) bor B3,
            Celsius = raw_to_celsius(Raw),
            {ok, Celsius};
        Error ->
            {error, Error}
    end.

%% Converts seesaw STATUS_TEMP raw value (signed 32-bit fixed-point) to °C.
-spec raw_to_celsius(integer()) -> float().
raw_to_celsius(Raw) when Raw >= 16#80000000 ->
    (Raw - 16#100000000) / 65536.0;
raw_to_celsius(Raw) ->
    Raw / 65536.0.