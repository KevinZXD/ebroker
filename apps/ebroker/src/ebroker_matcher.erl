%%%-------------------------------------------------------------------
%%% @author xudong12
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 7月 2020 6:41 下午
%%%-------------------------------------------------------------------
-module(ebroker_matcher).
-author("xudong12").
-include("ebroker.hrl").

%% API
-export([match_location/1]).

-spec match_location(string()) -> nomatch | {match, Location::#location{}, Captured::list()}.
match_location(Url) when is_list(Url) ->
	{ok, Locations} = ebroker_locations_server:get_locations(),
	match_location0(Url, Locations).
match_location0(_Url, []) ->
	nomatch;
match_location0(Url, [Location=#location{url_regexp_mp = MP}|Tail]) ->
	case re:run(Url, MP, [{capture, all, list}]) of
		nomatch ->
			match_location0(Url, Tail);
		{match, [_|Captured]} ->
			{match, Location, Captured}
	end.