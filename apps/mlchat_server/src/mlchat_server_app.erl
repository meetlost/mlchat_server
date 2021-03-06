%%%
%%% MLChat Server App
%%%

-module(mlchat_server_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("mlchat_server.hrl").

start(_Application, _Type) ->
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/", mlchat_server_h, []},
                                             {"/api/rooms", mlchat_server_h_chat_room, []},
                                             {"/ws/:chat_room/:username", mlchat_server_h_ws, []}
                                            ]}
                                     ]),
    {ok, IP} = application:get_env(?APP_NAME, ip),
    {ok, ConfigPort} = application:get_env(?APP_NAME, port),
    {ok, _} = cowboy:start_clear(http,
                                 [{ip, IP}, {port, ConfigPort}],
                                 #{env => #{dispatch => Dispatch}}),

    mlchat_server_db:start(),
    mlchat_server_sup:start_link().

stop(_Application) ->
    mlchat_server_db:stop(),
    cowboy:stop_listener(http),
    ok.
