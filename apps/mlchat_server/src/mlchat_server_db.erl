%%%
%%% MLChat Server Database
%%%

-module(mlchat_server_db).

-export([start/0, stop/0]).
-export([get_chat_room_list/2, store_chat_room/2]).

-include_lib("stdlib/include/qlc.hrl").

-record(chat_room, {name, intro}).

start() ->
    stop(), % Make sure mnesia:create_schema/1 always works.
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(chat_room,
                        [
                         {type, ordered_set},
                         {disc_copies, [node()]},
                         {attributes, record_info(fields, chat_room)}
                        ]).

stop() ->
    mnesia:stop().

%% Get Chat Room List
-spec get_chat_room_list(PageNumber, PageSize) -> {ok, ChatRoomList, Total} | {error, Reason} when
      PageNumber :: pos_integer(),
      PageSize :: pos_integer(),
      ChatRoomList :: [{Name :: atom(), Intro :: [term()]}],
      Total :: non_neg_integer(),
      Reason :: term().

get_chat_room_list(PageNumber, PageSize) ->
    F = fun() ->
                QH = qlc:q([{E#chat_room.name, E#chat_room.intro} || E <- mnesia:table(chat_room)]),
                QC = qlc:cursor(QH),
                Total = length(qlc:e(QH)),
                if
                    PageNumber =:= 1 ->
                        {qlc:next_answers(QC, PageSize), Total};
                    PageNumber > 1 ->
                        qlc:next_answers(QC, (PageNumber - 1) * PageSize),
                        {qlc:next_answers(QC, PageSize), Total};
                    true ->
                        {[], Total}
                end
        end,
    case mnesia:transaction(F) of
        {atomic, {RoomList, Total}} ->
            {ok, RoomList, Total};
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
                QH = qlc:q([{E#chat_room.name, E#chat_room.intro} || E <- mnesia:table(chat_room)]),
                Total = length(qlc:e(QH)),
                if
                    Total + 1 > 1000 ->
                        mnesia:abort('Too Many Chat Room');
                    true ->
                        mnesia:write(#chat_room{name = Name, intro = Intro})
                end
        end,
    case mnesia:transaction(F) of
        {atomic, _Res} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.
