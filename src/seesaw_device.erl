-module(seesaw_device).

-moduledoc """
Serialized I2C "device" for one seesaw bus+address (Erlang equivalent of
[Adafruit BusDevice I2CDevice](https://github.com/adafruit/Adafruit_CircuitPython_BusDevice)).

Holds the bus and address and runs one read/write at a time, so multiple
callers don't interleave I2C transactions. Use this when several processes
talk to the same sensor; otherwise `seesaw:read` / `seesaw:write` are enough.

### Example

```erlang
{ok, Pid} = seesaw_device:start_link(i2c1, 16#36),
seesaw_device:read_register(Pid, 16#0F, 16#10, 2).  %% moisture, 2 bytes
```
""".

-behaviour(gen_server).

-export([start_link/0, start_link/2, read_register/4, write_register/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(DEFAULT_BUS, i2c1).
-define(DEFAULT_ADDR, 16#36).

start_link() ->
    start_link(?DEFAULT_BUS, ?DEFAULT_ADDR).

-doc """
Starts a process that owns the I2C bus and device address and serializes
read/write. Use the returned Pid for read_register / write_register.
""".
-spec start_link(Bus :: term(), Addr :: 0..127) ->
          {ok, pid()} | {error, term()}.
start_link(Bus, Addr) ->
    gen_server:start_link(?MODULE, {Bus, Addr}, []).

-doc """
Reads Length bytes from register (RegBase, Reg). Serialized with other
calls to this device process.
""".
-spec read_register(pid(), 0..255, 0..255, pos_integer()) ->
          {ok, binary()} | {error, term()}.
read_register(Pid, RegBase, Reg, Length) ->
    gen_server:call(Pid, {read, RegBase, Reg, Length}, 5000).

-doc """
Writes Payload to register (RegBase, Reg). Serialized with other calls.
""".
-spec write_register(pid(), 0..255, 0..255, binary() | [byte()]) ->
          ok | {error, term()}.
write_register(Pid, RegBase, Reg, Payload) when is_list(Payload) ->
    write_register(Pid, RegBase, Reg, list_to_binary(Payload));
write_register(Pid, RegBase, Reg, Payload) when is_binary(Payload) ->
    gen_server:call(Pid, {write, RegBase, Reg, Payload}, 5000).

%% gen_server callbacks
init({Bus, Addr}) ->
    Ref = grisp_i2c:open(Bus),
    {ok, #{bus => Bus, addr => Addr, ref => Ref}}.

handle_call({read, RegBase, Reg, Length}, _From, State) ->
    #{ref := Ref, addr := Addr} = State,
    Cmd = <<RegBase:8, Reg:8>>,
    Result = case grisp_i2c:transfer(Ref, [
        {write, Addr, 0, Cmd},
        {read, Addr, 0, Length}
    ]) of
        [ok, Data] when is_binary(Data) ->
            {ok, Data};
        Other ->
            {error, Other}
    end,
    {reply, Result, State};

handle_call({write, RegBase, Reg, Payload}, _From, State) ->
    #{ref := Ref, addr := Addr} = State,
    Data = <<RegBase:8, Reg:8, Payload/binary>>,
    Result = case grisp_i2c:transfer(Ref, [{write, Addr, 0, Data}]) of
        [ok] ->
            ok;
        Other ->
            {error, Other}
    end,
    {reply, Result, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
