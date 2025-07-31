-module(eustaquia_sup).

-moduledoc """
The top-level supervisor for the Eustaquia application.

This supervisor is responsible for starting and monitoring
all main child processes of the system. Currently, it has
no children defined, but serves as the main entry point
for future worker or supervisor processes that make up
the plant monitoring and servo control logic.

It uses the `one_for_all` strategy, meaning if one child
process terminates, all other processes under this supervisor
will be terminated and then restarted together.
""".

-behavior(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-doc """
Starts the Eustaquia supervisor and links it to the calling process.

This is usually called by the application module at startup.
""".
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-doc """
Supervisor callback for initializing child processes.

The current implementation defines no child processes yet,
but uses a `one_for_all` strategy with 0 restarts in 1 second.
""".
-spec init(Args :: term()) ->
          {ok, {{RestartStrategy :: supervisor:strategy(),
                 MaxRestarts :: non_neg_integer(),
                 MaxSeconds :: non_neg_integer()},
                 [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
    {ok, {{one_for_all, 0, 1}, []}}.