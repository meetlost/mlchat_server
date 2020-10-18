%%% MLChat Server Chat Room Handler

-module(mlchat_server_h_chat_room).

%% Cowboy Callbacks
-export([ init/2 ]).
-export([ allowed_methods/2 ]).
-export([ content_types_provided/2 ]).
-export([ content_types_accepted/2 ]).

-export([ rooms/2 ]).

-include("mlchat_server.hrl").

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[ <<"GET">>, <<"POST">> ], Req, State}.

content_types_provided(Req, State) ->
    {[ {<<"application/json">>, rooms} ], Req, State}.

content_types_accepted(Req, State) ->
    {[ {<<"application/json">>, rooms} ], Req, State}.

rooms(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> chat_room_list(Req, State);
        <<"POST">> -> chat_room_create(Req, State)
    end.

chat_room_list(Req, State) ->
    #{
      pageNumber := PageNumber,
      pageSize := PageSize
     } = cowboy_req:match_qs([ pageNumber, pageSize ], Req),
    PageNumber1 = binary_to_integer(PageNumber),
    PageSize1 = binary_to_integer(PageSize),

    Body = case mlchat_server_db:get_chat_room_list(PageNumber1, PageSize1) of
               {ok, RoomList, Total} ->
                   RoomList1 = [ #{ name => Name, intro => Intro } || {Name, Intro} <- RoomList ],
                   jsx:encode(#{
                                <<"code">> => ?RES_OK,
                                <<"res">> => #{
                                               <<"list">> => RoomList1,
                                               <<"total">> => Total
                                              }
                               });
               {error, Reason} ->
                   jsx:encode(#{
                                <<"code">> => ?RES_ERROR,
                                <<"message">> => Reason
                               })
           end,

    {Body, Req, State}.

chat_room_create(Req, State) ->
    {ok, Data, _} = cowboy_req:read_body(Req),
    #{ <<"name">> := Name, <<"intro">> := Intro } = jsx:decode(Data, []),
    chat_room_create(Req, State, Name, Intro).

chat_room_create(Req, State, Name, Intro) ->
    Body = case mlchat_server_db:store_chat_room(Name, Intro) of
               ok ->
                   jsx:encode(#{ <<"code">> => ?RES_OK });
               {error, Reason} ->
                   jsx:encode(#{
                                <<"code">> => ?RES_ERROR,
                                <<"message">> => Reason
                               })
           end,
    Req1 = cowboy_req:set_resp_header(<<"content-type">>, "application/json", Req),
    Req2 = cowboy_req:set_resp_body(Body, Req1),

    {true, Req2, State}.
