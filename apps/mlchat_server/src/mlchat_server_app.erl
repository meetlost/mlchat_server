%%% MLChat Server App

-module(mlchat_server_app).

-behaviour(application).

-include("mlchat_server.hrl").

-export([start/2, stop/1]).

start(_Application, _Type) ->
    Dispatch = cowboy_router:compile([
                                      {'_',
                                       [
                                        {"/", mlchat_server_h, []},
                                        {"/api/rooms", mlchat_server_h_chat_room, []},
                                        {"/ws/:chat_room/:username", mlchat_server_h_ws, []}
                                       ]}
                                     ]),
    {ok, ConfigPort} = application:get_env(?APP_NAME, port),
    {ok, _} = cowboy:start_clear(http,
                                 [
                                  {port, ConfigPort}
                                 ],
                                 #{env => #{dispatch => Dispatch}}),

    mlchat_server_sup:start_link(),
    mlchat_server_db:start().

stop(_Application) ->
    cowboy:stop_listener(http),
    mlchat_server_db:stop(),
    ok.
