%%% MLChat Server WebSocket Handler

-module(mlchat_server_h_ws).

%% Cowboy Callbacks
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-export([chat_man/1]).

init(Req, State) ->
    ChatRoomName = cowboy_req:binding(chat_room, Req),
    ChatRoomName1 = list_to_atom(binary_to_list(ChatRoomName)),
    State1 = [{chat_room, ChatRoomName1} | State],

    UserName = cowboy_req:binding(username, Req),
    UserName1 = list_to_atom(binary_to_list(UserName)),
    State2 = [{username, UserName1} | State1],

    %% Idle timeout: infinity.
    {cowboy_websocket, Req, State2, #{idle_timeout => infinity}}.

websocket_init(State) ->
    {chat_room, ChatRoomName} = lists:keyfind(chat_room, 1, State),
    {username, UserName} = lists:keyfind(username, 1, State),
    init_chat_man(ChatRoomName, UserName),
    {[], State}.

websocket_handle({text, Msg}, State) ->
    {chat_room, ChatRoomName} = lists:keyfind(chat_room, 1, State),
    {username, UserName} = lists:keyfind(username, 1, State),

    %% Broadcast chat messages to all users.
    ChatRoomName ! {chat,
                    #{action => broadcast,
                      cmd => <<"chat">>,
                      content => Msg,
                      chat_room => ChatRoomName,
                      username => UserName,
                      origin => self()},
                    self()},

    {[], State};
websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    {[{text, Msg}], State};
websocket_info(#{cmd := CMD} = Info, State) ->
    %% Parse Data
    #{cmd := CMD,
      content := Msg,
      chat_room := ChatRoomName,
      username := UserName,
      origin := Origin} = Info,

    %% Send message to browser.
    Res = jsx:encode(#{<<"cmd">> => CMD,
                        <<"content">> => Msg,
                        <<"chat_room">> => list_to_binary(atom_to_list(ChatRoomName)),
                        <<"username">> => list_to_binary(atom_to_list(UserName)),
                        <<"self">> => Origin =:= self()
                       }),

    {[{text, Res}], State};
websocket_info(_Info, State) ->
    {[], State}.

terminate(_Reason, _PartialReq, State) ->
    {chat_room, ChatRoomName} = lists:keyfind(chat_room, 1, State),
    {username, UserName} = lists:keyfind(username, 1, State),
    goodbye_user(ChatRoomName, UserName, self()),
    ok.

%% Init Chat Man
-spec init_chat_man(ChatRoomName, UserName) -> ok when
      ChatRoomName :: atom(),
      UserName :: atom().

%% If the chat room related chat man does not exist, create one.
%% Or, tell chat man a new user comes in.
init_chat_man(ChatRoomName, UserName) ->
    case whereis(ChatRoomName) of
        undefined ->
            register(ChatRoomName, spawn(?MODULE, chat_man, [ChatRoomName])),
            welcome_new_user(ChatRoomName, UserName, self());
        _ChatRoomPid ->
            welcome_new_user(ChatRoomName, UserName, self())
    end,
    ok.

%% Chat Man
-spec chat_man(ChatRoomName) -> no_return() when
      ChatRoomName :: atom().

%% Every chat room has a specified chat man.
%% The chat man is responsible for broadcasting messages to users in the same chat room.
chat_man(ChatRoomName) ->
    logger:notice("New chat room \"~p\" initialized.", [ChatRoomName]),
    chat_man_loop(ChatRoomName, []).

%% Chat Man Loop
-spec chat_man_loop(ChatRoomName, UserList) -> no_return() when
      ChatRoomName :: atom(),
      UserList :: [pid()].

chat_man_loop(ChatRoomName, UserList) ->
    receive
        {new_user, #{username := UserName} = Msg, From} ->
            UserList1 = [#{username => UserName, user_pid => From} | UserList],
            logger:notice("Chat room: ~p, user list: ~p", [ChatRoomName, UserList1]),

            %% Broadcast new_user message to all users.
            [ UserPid ! Msg || #{user_pid := UserPid} <- UserList1 ],

            %% Update user list to all users.
            spawn(fun() -> update_user_list(ChatRoomName, UserList1) end),

            chat_man_loop(ChatRoomName, UserList1);
        {chat, Msg, _From} ->
            %% Broadcast chat message to all users.
            [ UserPid ! Msg || #{user_pid := UserPid} <- UserList ],

            chat_man_loop(ChatRoomName, UserList);
        {user_left, #{username := UserName} = Msg, From} ->
            UserList1 = lists:delete(#{username => UserName, user_pid => From}, UserList),
            logger:notice("Chat room: ~p, user list: ~p", [ChatRoomName, UserList1]),

            %% Broadcast user_left message to all users.
            [ UserPid ! Msg || #{user_pid := UserPid} <- UserList1 ],

            %% Update user list to all users.
            spawn(fun() -> update_user_list(ChatRoomName, UserList1) end),

            chat_man_loop(ChatRoomName, UserList1)
    end.

%% Welcome New User
-spec welcome_new_user(ChatRoomName, UserName, From) -> ok when
      ChatRoomName :: atom(),
      UserName :: atom(),
      From :: pid().

welcome_new_user(ChatRoomName, UserName, From) ->
    ChatRoomName ! {new_user,
                   #{action => broadcast,
                     cmd => <<"new_user">>,
                     content => <<"">>,
                     chat_room => ChatRoomName,
                     username => UserName,
                     origin => From},
                   From},
    ok.

%% Goodbye User
-spec goodbye_user(ChatRoomName, UserName, From) -> ok when
      ChatRoomName :: atom(),
      UserName :: atom(),
      From :: pid().

goodbye_user(ChatRoomName, UserName, From) ->
    ChatRoomName ! {user_left,
                    #{action => broadcast,
                      cmd => <<"user_left">>,
                      content => <<"">>,
                      chat_room => ChatRoomName,
                      username => UserName,
                      origin => From},
                    From},
    ok.

%% Update User List
-spec update_user_list(ChatRoomName, UserList) -> ok when
      ChatRoomName :: atom(),
      UserList :: [term()].

update_user_list(ChatRoomName, UserList) ->
    Content = [ [{username, UserName}] || #{username := UserName} <- UserList ],
    Content1 = jsx:encode(Content),
    [
     begin
         UserPid ! #{action => broadcast,
                     cmd => <<"user_list">>,
                     content => Content1,
                     chat_room => ChatRoomName,
                     username => UserName,
                     origin => self()}
     end || #{user_pid := UserPid, username := UserName} <- UserList
    ],
    ok.
