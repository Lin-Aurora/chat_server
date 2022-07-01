%%%-------------------------------------------------------------------
%%% @author lin
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 6月 2022 2:33 上午
%%%-------------------------------------------------------------------
-module(chat_client_test).
-export([start/0,loop/0,send_msg/1,send_msg/3,move_otp/2]).

start() ->
  {ok, Socket} = gen_tcp:connect("localhost", 8002, [binary, {packet,0}]),
  receive
    {tcp,_From,ID} ->
      io:format("您的用户id是: ~p~n",[binary_to_term(ID)])
  end,
  gen_tcp:send(Socket,term_to_binary("6666")),
  Pid =  spawn(fun() -> loop() end),
  gen_tcp:controlling_process(Socket, Pid),

  io:format("Pid = ~p~n",[Pid]),
  io:format("Socket = ~p~n",[Socket]).

loop() ->
  receive
    {tcp,_Socket,Msg} ->
      io:format("~ts~n",[binary_to_term(Msg)]),
      loop();
    {tcp_closed, _Socket} ->
      io:format("对方连接已关闭! ~n")
  end.

%% 私聊/群聊  ID为0表示群聊 ID为具体ID表示私聊
send_msg(Socket) ->
  ID = io:get_line("对方ID:"),
  Msg = io:get_line("消息内容:"),
  {Result, _Info} = string:to_integer(ID),
  gen_tcp:send(Socket,term_to_binary([Result,Msg])).
send_msg(Socket,ID,Msg) ->
  {Result, _Info} = string:to_integer(ID),
  gen_tcp:send(Socket,term_to_binary([Result,Msg])).

move_otp(Socket,Dir) ->
  {Result, _Info} = string:to_integer("-1"),
  gen_tcp:send(Socket,term_to_binary([Result,Dir])).