-module(servo_emo).

-moduledoc """
Servo control module for Eustaquia.

 This module abstracts controlling a small servo motor (e.g., SG90) via
 PWM to visually express the plant's mood:

 - 😀 Happy: Servo moves to a 180° position
 - 😢 Sad: Servo moves to a 0° position

 It uses the `grisp_pwm` driver to start, stop, and adjust PWM signals
 for the pin connected to the servo.

 Typical usage:
 ```
 servo_emo:start(gpio1_4),
 servo_emo:set_emo(gpio1_4, happy).
 ```
""".

-export([start/1, stop/1, set_emo/2]).

-doc """
Starts the PWM driver on a given pin, configured for standard hobby servos.

Initializes PWM at ~50 Hz for a servo connected to Pin. If the driver or
pin is already open, it will reuse the existing instance.
""".
-spec start(Pin :: atom()) -> ok | {error, Reason :: term()}.
start(Pin) ->
    _ = try
        _ = grisp_pwm:start_driver()
    catch
        error:{already_started, _Pid} ->
            io:format("PWM driver already started.~n"),
            ok;
        Class:Reason ->
            io:format("Error starting PWM driver: ~p~n", [{Class, Reason}]),
            {error, Reason}
    end,
    case grisp_pwm:open(Pin, {ipg_clk, 66, <<20000:16>>}, 0.0) of
        ok ->
            io:format("PWM started on pin ~p.~n", [Pin]);
        {error, already_open} ->
            io:format("PWM already open on pin ~p.~n", [Pin]);
        Error ->
            io:format("Error opening PWM on pin ~p: ~p~n", [Pin, Error])
    end.

-doc """
Stops PWM on the given pin.

Closes the PWM channel for Pin, freeing resources.
""".
-spec stop(Pin :: atom()) -> ok.
stop(Pin) -> grisp_pwm:close(Pin).

-doc """
Sets the servo’s emotional state or custom duty cycle.
	•	happy: Moves servo to 180° (duty cycle = 0.125)
	•	sad: Moves servo to 0° (duty cycle = 0.025)
	•	Any float: Directly sets the PWM duty cycle for fine control.

Hobby servos like SG90 respond to PWM signals at ~50 Hz.
Duty cycle values 0.025 and 0.125 correspond to the servo’s
rotational extremes (0° and 180°).
""".
-spec set_emo(Pin :: atom(), Emotion :: happy | sad | float()) -> ok.
set_emo(Pin, happy) -> grisp_pwm:set_sample(Pin, 0.125);
set_emo(Pin, sad) -> grisp_pwm:set_sample(Pin, 0.025);
set_emo(Pin, Duty) when is_float(Duty) -> grisp_pwm:set_sample(Pin, Duty).