%%%-------------------------------------------------------------------
%%% @author lin
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 6月 2022 4:27 下午
%%%-------------------------------------------------------------------
-module(gateway_app).
-author("lin").
%% API
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
%%start() ->
  %% 启动tcp_echo监听器
  { ok , _Socket } =  ranch:start_listener ( tcp_echo ,
    ranch_tcp, #{ socket_opts  => [{ port , 8002 }],
                  max_connections => infinity,
                  num_acceptors => erlang:system_info(schedulers_online)},
    chat_server_test , []
  ),
  case gateway_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

stop(State) ->
  State.