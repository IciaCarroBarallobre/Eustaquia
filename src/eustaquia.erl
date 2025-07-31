-module(eustaquia).

-moduledoc """
The main application module for Eustaquia.

This module handles the startup and shutdown of the application
and provides a simple public API to run the main humidity-check loop
and test servo behaviors.

Eustaquia periodically reads soil humidity via an I²C sensor and
moves a servo motor to visually indicate the plant's mood:

- 😀 Happy: Soil is moist
- 😢 Sad: Soil is dry

It also communicates with other distributed BEAM nodes running Eustaquia,
broadcasting a `waterpls` message when the plant is thirsty.
""".

-behavior(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Public API
-export([
    loop/0,
    test_moisture/0,
    test_servo_happy/0,
    test_servo_sad/0
]).

%% Constants
-define(THRESHOLD, 1000).    %% Humidity threshold
-define(INTERVAL, 5000).     %% Interval (ms) between readings
-define(PIN, gpio1_4).       %% Servo pin

-doc """
Starts the Eustaquia application.

This callback is invoked when the application is started
and it launches the top-level supervisor.
""".
-spec start(StartType :: term(), StartArgs :: term()) ->
          {ok, pid()} | {error, Reason :: term()}.
start(_Type, _Args) ->
    eustaquia_sup:start_link().

-doc """
Stops the Eustaquia application.

Called when the application is stopped.
""".
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.

-doc """
Starts the main event loop.

Initializes the servo motor and schedules periodic humidity checks
every `?INTERVAL` milliseconds. Based on the readings, the plant face
is updated to happy or sad.
""".
-spec loop() -> no_return().
loop() ->
    servo_emo:start(?PIN),
    erlang:send_after(?INTERVAL, self(), check_soil),
    wait().

%% Private: Internal receive loop
-doc false.
-spec wait() -> ok | no_return().
wait() ->
    receive
        {_PID, waterpls} ->
            io:format("😢 Another plant is speaking... I’m sad.~n"),
            servo_emo:set_emo(?PIN, sad),
            wait();

        check_soil ->
            case hum_sensor:read_moisture() of
                {ok, Moisture} when Moisture < ?THRESHOLD ->
                    io:format("😢 Soil is dry (~p).~n", [Moisture]),
                    servo_emo:set_emo(?PIN, sad),
                    lists:foreach(fun(Node) ->
                        {eustaquia, Node} ! waterpls
                    end, nodes()),
                    loop();

                {ok, Moisture} ->
                    io:format("😊 Soil is moist (~p).~n", [Moisture]),
                    servo_emo:set_emo(?PIN, happy),
                    loop();

                Error ->
                    io:format("⚠️ Error reading humidity: ~p~n", [Error]),
                    loop()
            end;

        stop_loop ->
            ok;

        _ ->
            wait()
    end.

-doc """
Reads moisture once and returns the sensor value.

Initializes the servo motor and performs a single humidity read.
""".
-spec test_moisture() -> {ok, integer()} | {error, term()}.
test_moisture() ->
    servo_emo:start(?PIN),
    hum_sensor:read_moisture().

-doc """
Moves the servo motor to the "happy" position.

Used for testing servo movement.
""".
-spec test_servo_happy() -> ok.
test_servo_happy() ->
    servo_emo:start(?PIN),
    servo_emo:set_emo(?PIN, happy).

-doc """
Moves the servo motor to the "sad" position.

Used for testing servo movement.
""".
-spec test_servo_sad() -> ok.
test_servo_sad() ->
    servo_emo:start(?PIN),
    servo_emo:set_emo(?PIN, sad).