%%%-------------------------------------------------------------------
%%% @author mac
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 6ζ 2022 11:42 δΈε
%%%-------------------------------------------------------------------
-module(chat_client_test_multi).
-author("mac").

%% API
-export([start/1]).

start(N) ->
  [loop() || _ <- lists:seq(1,N)].

loop() ->
  spawn(fun() -> chat_client_test:start() end).