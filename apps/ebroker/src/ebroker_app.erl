%%%-------------------------------------------------------------------
%% @doc ebroker public API
%% @end
%%%-------------------------------------------------------------------

-module(ebroker_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ebroker_upstreams_ets:start(),
    start_hackney_pools(),
    start_http_server(),
    ebroker_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% 启动连接池，一个 域名+端口 复用一组链接
start_hackney_pools() ->
    {ok, Upstreams} = application:get_env(ebroker, upstreams),
    [begin
         PoolSize = proplists:get_value(keepalive, Props),
         Servers = proplists:get_value(servers, Props),
         true = ebroker_upstreams_ets:add_pool(PoolName, Servers),
         Options = proplists:get_value(options, Props, [{timeout, 1500}, {max_connections, PoolSize}]),
         ok = hackney_pool:start_pool(PoolName, Options)
     end || {PoolName, Props} <- Upstreams],
    ok.

%% 启动http服务
start_http_server() ->
    {ok, Props}     = application:get_env(ebroker, http_server),
    Acceptors       = proplists:get_value(acceptors, Props, 50),
    MaxConnections  = proplists:get_value(max_connections, Props, 10240),
    Backlog         = proplists:get_value(backlog, Props, 1024),
    Port            = proplists:get_value(port, Props),

    Dispatcher = cowboy_router:compile([
        {'_', [{'_', http_handler, [http]}]}
    ]),
    TransOpts = [
        {port, Port},
        {num_acceptors, Acceptors},
        {backlog, Backlog},
        {max_connections, MaxConnections}
    ],
    {ok, _} = cowboy:start_clear(http_listener, TransOpts, #{env => #{dispatch => Dispatcher}}),
    lager:info("[eparallel_app] the http server start at: ~p", [Port]).