%%%-------------------------------------------------------------------
%%% @author lin
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 6月 2022 3:23 下午
%%%-------------------------------------------------------------------
-module(pressure).
-author("lin").
%% API
-export([generate/1,rand_send/1,rand_move/1,rand_user/2,time/2,test/1]).

%% 用户每N毫秒随机移动或说话
rand_user(Socket,N) ->
  rand_move(Socket),
  rand_send(Socket),
  timer:sleep(N),
  rand_user(Socket,N).


rand_move(Socket) ->
  Dir = ["up","down","right","left"],
  chat_client_test:move_otp(Socket,lists:nth(rand:uniform(4),Dir)).

rand_send(Socket) ->
  chat_client_test:send_msg(Socket,"0","-------test------").

generate(N) ->
  [spawn(chat_client_test,start,[]) || _ <- lists:seq(1,N)].

test(N) ->
  {U1,N} = time(N,1000),
  U2 = max_move(N),
  U3 = max_send(N),
  io:format("玩家平均单个进程消耗： ~p 微秒~n目前可运行进程数量为： ~p 个~n目前玩家随机移动进程消耗时间为： ~p 微秒~n目前玩家随机发言进程消耗时间为： ~p 微秒~n",[trunc(U1),N,trunc(U2),trunc(U3)]).

time(N,T) ->
  statistics(runtime),
  L = for(1,N,fun() -> spawn(fun() ->
                                Socket = chat_client_test:start(),
                                io:format("当前运行进程：~p  当前玩家进程每 ~p 毫秒随机移动和发言~n",[self(), T]),
                                rand_user(Socket,1000)
                             end) end),
  {_,Time1} = statistics(runtime),
  lists:foreach(fun(Pid) -> Pid ! die end,L),
  U1 = Time1 * 1000 / N,
  io:format("总共运行 ~p 个进程  玩家平均单个进程时间消耗：~p 微秒~n",[N,U1]).

for(N,N,F) -> [F()];
for(I,N,F) -> [F()|for(I + 1,N,F)].

max_move(N) ->
  statistics(runtime),
  L = for(1,N,fun() -> spawn(fun() -> rand_move(chat_client_test:start()) end) end),
  {_,Time1} = statistics(runtime),
  lists:foreach(fun(Pid) -> Pid ! die end,L),
  U2 = Time1 * 1000 / N,
  U2.

max_send(N) ->
  statistics(runtime),
  L = for(1,N,fun() -> spawn(fun() -> rand_send(chat_client_test:start()) end) end),
  {_,Time1} = statistics(runtime),
  lists:foreach(fun(Pid) -> Pid ! die end,L),
  U3 = Time1 * 1000 / N,
  U3.