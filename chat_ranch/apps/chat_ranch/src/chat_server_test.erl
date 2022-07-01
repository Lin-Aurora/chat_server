%%%-------------------------------------------------------------------
%%% @author lin
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 6月 2022 4:24 下午
%%%-------------------------------------------------------------------
-module(chat_server_test).
-author("lin").

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/3, all_send/1, move_otp/2, room_location_init/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 50000).

-record(otp_server_state, {}).
-record(state,{socket,transport}).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Ref::ranch:ref(),
    Transport::module(),
    ProtocolOptions::any()) ->
  {ok, ConnectionPid::pid()}
  | {ok, SupPid::pid(), ConnectionPid::pid()}).
start_link(Ref, Transport, Opts) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Transport, Opts}])}.
%%  {ok, proc_lib:start_link(?MODULE, init, [{Ref, Transport, Opts}])}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #otp_server_state{}} | {ok, State :: #otp_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init({Ref, Transport, _Opts = []}) ->
  {ok, Socket} = ranch:handshake(Ref),
  ok = Transport:setopts(Socket, [{active, once}]),
%%  spawn(fun() -> loop(Socket,Transport) end),
  gen_server:enter_loop(?MODULE, [], #state{socket = Socket, transport = Transport}),
  {ok,#state{socket = ranch:handshake(Ref), transport = Transport}}.


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #otp_server_state{}) ->
  {reply, Reply :: term(), NewState :: #otp_server_state{}} |
  {reply, Reply :: term(), NewState :: #otp_server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #otp_server_state{}} |
  {noreply, NewState :: #otp_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #otp_server_state{}} |
  {stop, Reason :: term(), NewState :: #otp_server_state{}}).

%% 移动
handle_call({move,Socket,Dir},_From,Tab) ->
  [{_,Id}] = ets:lookup(name,Socket),
  [{_,{X,Y}}] = ets:lookup(location,Socket),
  Reply =   case Dir of
              "up" when Y < 50 ->
                io:format("用户~p  移动成功！当前位置在: [~p,~p]\n" , [Id,X,Y+1]),
                ets:insert(location,{Socket,{X,Y+1}}),
                move_msg(Socket,"  用户" ++ integer_to_list(Id) ++ " 向上移动到 [" ++ integer_to_list(X) ++ "," ++ integer_to_list(Y+1) ++ "]");
              "down" when Y > 0 ->
                io:format("用户~p  移动成功！当前位置在: [~p,~p]\n" , [Id,X,Y-1]),
                ets:insert(location,{Socket,{X,Y-1}}),
                move_msg(Socket,"  用户" ++ integer_to_list(Id) ++ " 向下移动到 [" ++ integer_to_list(X) ++ "," ++ integer_to_list(Y-1) ++ "]");
              "left" when X > 0 ->
                io:format("用户~p  移动成功！当前位置在: [~p,~p]\n", [Id,X-1,Y]),
                ets:insert(location,{Socket,{X-1,Y}}),
                move_msg(Socket,"  用户" ++ integer_to_list(Id) ++ " 向左移动到 [" ++ integer_to_list(X-1) ++ "," ++ integer_to_list(Y+1) ++ "]");
              "right" when X < 50 ->
                io:format("用户~p  移动成功！当前位置在: [~p,~p]\n" , [Id,X+1,Y]),
                ets:insert(location,{Socket,{X+1,Y}}),
                move_msg(Socket,"  用户" ++ integer_to_list(Id) ++ " 向右移动到 [" ++ integer_to_list(X+1) ++ "," ++ integer_to_list(Y+1) ++ "]");
              _ ->
                io:format("移动失败！当前位置所处方向不可移动！\n")
            end,
  {reply,Reply,Tab}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #otp_server_state{}) ->
  {noreply, NewState :: #otp_server_state{}} |
  {noreply, NewState :: #otp_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #otp_server_state{}}).
handle_cast(_Request, State = #otp_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #otp_server_state{}) ->
  {noreply, NewState :: #otp_server_state{}} |
  {noreply, NewState :: #otp_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #otp_server_state{}}).
handle_info({tcp, Socket, Data}, State = #state{socket = Socket, transport = TransPort}) ->
  Response = Data,
  TransPort:setopts(Socket, [{active, once}]),
  TransPort:send(Socket, Response),
  {noreply, State, ?TIMEOUT};
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
  {stop, Reason, State};
handle_info(timeout, State) ->
  {stop, normal, State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #otp_server_state{}) -> term()).
terminate(_Reason, _State = #otp_server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #otp_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #otp_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #otp_server_state{}, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
loop(Socket, Transport) ->
  io:format("Accept Socket = ~p~n",[Socket]),
  timer:sleep(100),
  case ets:last(user) of
    '$end_of_table' ->
      ets:insert(user,{1,Socket}),
      ets:insert(name,{Socket,1}),
      ranch_tcp:send(Socket,term_to_binary(1));
    Other ->
      ets:insert(user,{Other+1,Socket}),
      ets:insert(name,{Socket,Other + 1}),
      ranch_tcp:send(Socket,term_to_binary(Other+1))
  end,
  room_location_init(Socket),
  spawn(fun() -> loop(Socket) end),

  case Transport:recv(Socket, 0, 5000) of
    {ok, Data} ->
      Transport:send(Socket, Data),
      loop(Socket, Transport);
    _ ->
      ok = Transport:close(Socket)
  end.

loop(Socket) ->
    receive
    {tcp, Socket, Bin} ->
      [ID, Msg] = binary_to_term(Bin),
      case ID of
        0 ->
          [{_,UID}] = ets:lookup(name,Socket),
          Send_Msg = calendar:system_time_to_rfc3339(os:system_time(second), [{second}]) ++ "  用户" ++ integer_to_list(UID) ++ ": " ++ Msg,
          ?MODULE:all_send(Send_Msg),
          ranch_tcp:send(Socket,term_to_binary(Send_Msg)),
          loop(Socket);
        -1 ->
          move_otp(Socket,Msg),
          loop(Socket);
        _ ->
          [{ID,SocketNum}] = ets:lookup(user,ID),
          [{_,UID}] = ets:lookup(name,Socket),
          Send_Msg = calendar:system_time_to_rfc3339(os:system_time(second), [{second}]) ++ "  用户" ++ integer_to_list(UID) ++ ": " ++ Msg,
          ranch_tcp:send(Socket,term_to_binary(Send_Msg)),
          ranch_tcp:send(SocketNum,term_to_binary(Send_Msg)),
          loop(Socket)
      end;
    {tcp_closed, Socket} ->
      io:format("连接已关闭！ ~n")
  end.

%% 频道聊天
all_send(Msg) ->
    [gen_tcp:send(Socket,term_to_binary(Msg)) || {_P,Socket} <- ets:tab2list(user)].

%% 移动发送信息
move_msg(Socket,Msg) ->
  Msg1 = calendar:system_time_to_rfc3339(os:system_time(second), [{second}]) ++ Msg,
  {Result, _Info} = string:to_integer("0"),
  gen_tcp:send(Socket,term_to_binary([Result,Msg1])).

%% 发起移动请求
move_otp(Socket,Dir) ->
  gen_server:call(?MODULE,{move,Socket,Dir}).

%% 新用户房间初始化
room_location_init(Socket) ->
  RoomId = rand:uniform(10),
  X = rand:uniform(50) - 1,
  Y = rand:uniform(50) - 1,
  ets:insert(room,{Socket,RoomId}),
  ets:insert(location,{Socket,{X,Y}}),
  ranch_tcp:send(Socket,term_to_binary("当前您所处的房间是: " ++ integer_to_list(RoomId) ++ " ，您所在的位置是: [" ++ integer_to_list(X) ++ "," ++ integer_to_list(Y) ++"]")),
  io:format("当前新用户所处的房间是: " ++ integer_to_list(RoomId) ++ " ，新用户所在的位置是: [" ++ integer_to_list(X) ++ "," ++ integer_to_list(Y) ++"]\n").
