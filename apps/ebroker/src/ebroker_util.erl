%%%-------------------------------------------------------------------
%%% @author licheng5
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 7月 2020 1:03 上午
%%%-------------------------------------------------------------------
-module(ebroker_util).
-author("licheng5").

%% API
-export([timestamp/0, local_date_string/0]).

timestamp() ->
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	1000000 * MegaSecs + Secs + MicroSecs / 1000000.

local_date_string() ->
	Timestamp = {_, _, MicroSecs} = os:timestamp(),
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
	lists:flatten(
		io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0w",
			[Year, Month, Day, Hour, Minute, Second, MicroSecs div 1000])).

