%%%-------------------------------------------------------------------
%% @doc chat_ranch public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_ranch_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
%%    application:ensure_all_started(ranch),
%%    application:start(gateway_app),

    %% 启动tcp_echo监听器
    { ok , _Socket } =  ranch:start_listener ( tcp_echo ,
        ranch_tcp, #{ socket_opts  => [{ port , 8002 }],
            max_connections => infinity,
            num_acceptors => erlang:system_info(schedulers_online)},
        chat_server_test , []
    ),
    gateway_sup:start_link(),
%%    case gateway_sup:start_link() of
%%        {ok, Pid} ->
%%            {ok, Pid};
%%        Error ->
%%            Error
%%    end,

    init_ets(),
    chat_ranch_sup:start_link().

stop(_State) ->
    application:stop(ranch),
    application:stop(gateway_app).

%% internal functions
init_ets() ->
    case lists:member(name, ets:all()) of
        false -> ets:new(name,[ordered_set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> void
    end,
    case lists:member(room, ets:all()) of
        false -> ets:new(room,[set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> void
    end,
    case lists:member(location, ets:all()) of
        false -> ets:new(location,[set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> void
    end,
    case lists:member(user, ets:all()) of
        false -> ets:new(user,[set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> void
    end.