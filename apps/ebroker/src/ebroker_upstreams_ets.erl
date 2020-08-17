%%%-------------------------------------------------------------------
%%% @author xudong12
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 4月 2020 10:58 上午
%%%-------------------------------------------------------------------
-module(ebroker_upstreams_ets).
-author("xudong12").
-define(TAB_NAME, ebroker_upstreams_ets_tab).

%% API
-export([start/0, add_pool/2, get_server/1, all/0]).

start() ->
	ets:new(?TAB_NAME, [named_table, ordered_set, public, {read_concurrency, true}]).

add_pool(PoolName, Servers) when is_atom(PoolName), is_list(Servers) ->
	ets:insert(?TAB_NAME, {PoolName, Servers}).

get_server(PoolName) when is_atom(PoolName) ->
	case ets:lookup(?TAB_NAME, PoolName) of
		[{_, Servers}] ->
			case length(Servers) =:= 1 of
				true ->
					{ok, hd(Servers)};
				false ->
					Idx = rand:uniform(length(Servers)),
					{ok, lists:nth(Idx, Servers)}
			end;
		_ ->
			undefined
	end.

all() ->
	ets:tab2list(?TAB_NAME).