-module(hum_sensor).

-moduledoc """
Module for reading soil moisture from a capacitive sensor via I2C.

This module communicates with an Adafruit soil moisture sensor
(or similar) connected to the GRiSP board. It uses the `grisp_i2c`
library to send a read command and retrieve a 16-bit moisture value.

### Example usage:

```erlang
1> hum_sensor:read_moisture().
Moisture: 850
{ok, 850}
```
""".

-export([read_moisture/0]).

-define(I2C_BUS, i2c1).          %% I2C bus name
-define(I2C_ADDR, 16#36).        %% Device address (0x36)

-define(TOUCH_BASE, 16#0F).      %% Base register for reading
-define(TOUCH_FUNC, 16#10).      %% Function code for measuring

-doc """
Reads soil moisture from the I2C sensor.

Opens the I2C bus, sends a command to the sensor, and reads
a 2-byte value representing the soil moisture level.

Returns:
- {ok, Moisture}: The measured humidity value (0-65535)
- {error, Reason}: If communication fails.
""".
-spec read_moisture() -> {ok, non_neg_integer()} | {error, term()}.
read_moisture() ->
    Ref = grisp_i2c:open(?I2C_BUS),
    Cmd = <<?TOUCH_BASE, ?TOUCH_FUNC>>,
    case grisp_i2c:transfer(Ref, [
        {write, ?I2C_ADDR, 0, Cmd},
        {read, ?I2C_ADDR, 0, 2}
    ]) of
        [ok, <<Hi:8, Lo:8>>] ->
            Moisture = (Hi bsl 8) bor Lo,
            io:format("Moisture: ~p~n", [Moisture]),
            {ok, Moisture};
        Error ->
            io:format("Error reading moisture: ~p~n", [Error]),
            {error, Error}
    end.