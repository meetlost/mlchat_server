%%% MLChat Server Database

-module(mlchat_server_db).

-export([start/0, stop/0]).
-export([get_chat_room_list/2, store_chat_room/2]).

-include_lib("stdlib/include/qlc.hrl").

-record(chat_room, {name, intro}).

start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(chat_room,
                        [
                         {type, set},
                         {disc_copies, [node()]},
                         {attributes, record_info(fields, chat_room)}
                        ]).

stop() ->
    mnesia:stop().

%% Get Chat Room List
-spec get_chat_room_list(Start, Len) -> {ok, ChatRoomList, Total} | {error, Reason} when
      Start :: pos_integer(),
      Len :: pos_integer(),
      ChatRoomList :: [{Name :: atom(), Intro :: [term()]}],
      Total :: non_neg_integer(),
      Reason :: term().

get_chat_room_list(Start, Len) ->
    F = fun() ->
                QH = qlc:q([ {E#chat_room.name, E#chat_room.intro} || E <- mnesia:table(chat_room) ]),
                QC = qlc:cursor(QH),
                if
                    Start =:= 1 ->
                        qlc:next_answers(QC, Len);
                    Start > 1 ->
                        qlc:next_answers(QC, Start - 1),
                        qlc:next_answers(QC, Len);
                    true ->
                        []
                end
        end,
    case mnesia:transaction(F) of
        {atomic, Res} ->
            {ok, Res, length(Res)};
        {aborted, Reason} ->
            {error, Reason}
    end.

%% Store Chat Room
-spec store_chat_room(Name, Intro) -> ok | {error, Reason} when
      Name :: atom(),
      Intro :: [term()],
      Reason :: term().

store_chat_room(Name, Intro) ->
    F = fun() ->
                mnesia:write(#chat_room{name = Name, intro = Intro})
        end,
    case mnesia:transaction(F) of
        {atomic, _Res} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.
