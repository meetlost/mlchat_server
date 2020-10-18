%%% MLChat Server Handler

-module(mlchat_server_h).

%% Cowboy Callbacks
-export([ init/2 ]).
-export([ content_types_provided/2 ]).

-export([ hello/2 ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[ {<<"text/html">>, hello} ], Req, State}.

hello(Req, State) ->
    Body = <<"hello, world">>,
    {Body, Req, State}.
