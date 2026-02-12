-module(seesaw).

-moduledoc """
Generic seesaw protocol over I2C for GRiSP.

Implements the same register read/write protocol as Adafruit's seesaw devices
(ATSAMD09/10, ATtiny, etc.). Register map matches the
[seesaw firmware](https://github.com/adafruit/seesaw) (see `include/RegisterMap.h`).
Host side adapted from the
[Adafruit CircuitPython seesaw](https://github.com/adafruit/Adafruit_CircuitPython_seesaw)
and [Adafruit_Seesaw Arduino](https://github.com/adafruit/Adafruit_Seesaw)
libraries: you write a 2-byte register address (module base + register), then
optionally read or write data. Use `read/6` with a delay when the chip needs
time to sample (e.g. temperature 100–1000 ms, touch 10–100 ms). GRiSP I2C PMOD.

### Relation to Adafruit BusDevice

[Adafruit CircuitPython BusDevice](https://github.com/adafruit/Adafruit_CircuitPython_BusDevice)
provides `I2CDevice`: it holds the bus + address and **locks the bus** so only
one transaction runs at a time. In Erlang we don't lock by default: each
`read`/`write` does its own open+transfer. If only one process talks to the
sensor, that's fine. For concurrent access, start `seesaw_device` (see the `seesaw_device` module):
it serializes all requests for one bus+address, like BusDevice's lock.

### Protocol (matches CircuitPython)

- **Read**: write `[RegBase, Reg]`, then read N bytes from the device.
- **Write**: write `[RegBase, Reg, Payload...]`.

### Example

```erlang
%% Read 2 bytes from touch channel 0 (e.g. moisture)
seesaw:read(i2c1, 16#36, 16#0F, 16#10, 2).

%% Read 4 bytes from status temperature register
seesaw:read(i2c1, 16#36, 16#00, 16#04, 4).
```
""".

-export([read/6, read/5, read/3, write/5, write/3]).

-define(DEFAULT_BUS, i2c1).
-define(DEFAULT_ADDR, 16#36).

%% Status module (Arduino Adafruit_Seesaw)
-define(STATUS_BASE, 16#00).
-define(STATUS_HW_ID, 16#01).
-define(STATUS_VERSION, 16#02).
-define(STATUS_SWRST, 16#7F).

-doc """
Reads Length bytes from a seesaw register.

Same as CircuitPython `read(reg_base, reg, buf)`: sends [RegBase, Reg],
then reads Length bytes. Uses default bus `i2c1` and address `0x36`.
""".
-spec read(RegBase :: 0..255, Reg :: 0..255, Length :: pos_integer()) ->
          {ok, binary()} | {error, term()}.
read(RegBase, Reg, Length) ->
    read(?DEFAULT_BUS, ?DEFAULT_ADDR, RegBase, Reg, Length).

-doc """
Reads Length bytes from a seesaw register on the given I2C bus and address.

Returns {ok, Binary} with the raw bytes (big-endian as from the device)
or {error, Reason} if the transfer fails.
""".
-spec read(Bus :: term(), Addr :: 0..127, RegBase :: 0..255, Reg :: 0..255,
           Length :: pos_integer()) ->
          {ok, binary()} | {error, term()}.
read(Bus, Addr, RegBase, Reg, Length) ->
    read(Bus, Addr, RegBase, Reg, Length, 0).

-doc """
Reads Length bytes after an optional delay (like Arduino read(..., delay)).

The Arduino library uses a delay between writing the register and reading so
the chip can complete the conversion (e.g. getTemp uses 1000 ms, touchRead
uses 3000+ ms). DelayMs is in milliseconds; 0 means no delay.
""".
-spec read(Bus :: term(), Addr :: 0..127, RegBase :: 0..255, Reg :: 0..255,
           Length :: pos_integer(), DelayMs :: non_neg_integer()) ->
          {ok, binary()} | {error, term()}.
read(Bus, Addr, RegBase, Reg, Length, DelayMs) ->
    Ref = grisp_i2c:open(Bus),
    Cmd = <<RegBase:8, Reg:8>>,
    case grisp_i2c:transfer(Ref, [{write, Addr, 0, Cmd}]) of
        [ok] when DelayMs > 0 ->
            timer:sleep(DelayMs),
            do_read(Ref, Addr, Length);
        [ok] ->
            do_read(Ref, Addr, Length);
        Other ->
            {error, Other}
    end.

do_read(Ref, Addr, Length) ->
    case grisp_i2c:transfer(Ref, [{read, Addr, 0, Length}]) of
        [ok, Data] when is_binary(Data) ->
            {ok, Data};
        Other ->
            {error, Other}
    end.

-doc """
Writes Payload to a seesaw register.

Same as CircuitPython `write(reg_base, reg, buf)`: sends [RegBase, Reg, Payload].
Uses default bus and address.
""".
-spec write(RegBase :: 0..255, Reg :: 0..255, Payload :: binary() | [byte()]) ->
          ok | {error, term()}.
write(RegBase, Reg, Payload) ->
    write(?DEFAULT_BUS, ?DEFAULT_ADDR, RegBase, Reg, Payload).

-doc """
Writes Payload to a seesaw register on the given bus and address.
""".
-spec write(Bus :: term(), Addr :: 0..127, RegBase :: 0..255, Reg :: 0..255,
            Payload :: binary() | [byte()]) ->
          ok | {error, term()}.
write(Bus, Addr, RegBase, Reg, Payload) when is_list(Payload) ->
    write(Bus, Addr, RegBase, Reg, list_to_binary(Payload));
write(Bus, Addr, RegBase, Reg, Payload) when is_binary(Payload) ->
    Ref = grisp_i2c:open(Bus),
    Data = <<RegBase:8, Reg:8, Payload/binary>>,
    case grisp_i2c:transfer(Ref, [{write, Addr, 0, Data}]) of
        [ok] ->
            ok;
        Other ->
            {error, Other}
    end.
