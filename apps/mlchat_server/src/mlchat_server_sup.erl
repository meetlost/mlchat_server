%%% MLChat Server Supervisor

-module(mlchat_server_sup).

-behaviour(supervisor).

%% API
-export([ start_link/0 ]).

%% Supervisor Callbacks
-export([ init/1 ]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
                 strategy => one_for_one,
                 intensity => 10,
                 period => 10
                },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
