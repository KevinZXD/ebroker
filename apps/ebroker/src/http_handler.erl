%%%-------------------------------------------------------------------
%%% @author xudong12
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 4月 2020 3:36 下午
%%%-------------------------------------------------------------------
-module(http_handler).
-author("xudong12").
-include("ebroker.hrl").
-define(MAX_TIMEOUT, 5000).

%% API
-export([init/2]).

init(Req0, Opts) ->
	Path = binary_to_list(cowboy_req:path(Req0)),

	Date = ebroker_util:local_date_string(),
	StartTs = ebroker_util:timestamp(),
	{IP, _} = cowboy_req:peer(Req0),
	Method = binary_to_list(cowboy_req:method(Req0)),

	lager:debug("[http_handler] get a new connection, sock: ~p, path: ~p", [cowboy_req:peer(Req0), Path]),
	case ebroker_matcher:match_location(Path) of
		nomatch ->
			Req1 = cowboy_req:reply(404, #{
				<<"content-type">> => <<"text/plain; charset=utf-8">>
			}, <<"Not Found">>, Req0),

			access_log(Date, Method, Path, 404, IP, ebroker_util:timestamp() - StartTs),

			{ok, Req1, Opts};
		{match, Location, Captured} ->
			case forward_request(Req0, Location, Captured) of
				{ok, Req1, {ok, StatusCode, RespHeaders, RespBody}} ->
					%% 返回的数据的body编码已经不是chunked了
					RemoveHeaderKeys = [
						<<"transfer-encoding">>,
						<<"connection">>
					],
					RespHeaders1 = lists:filter(fun({Key, _}) ->
						Key1 = list_to_binary(string:to_lower(binary_to_list(Key))),
						not lists:member(Key1, RemoveHeaderKeys)
					                            end, RespHeaders),
					RespHeaders2 = RespHeaders1 ++ [
						{<<"connection">>, <<"keep-alive">>}
					],
					Req2 = cowboy_req:reply(StatusCode, maps:from_list(RespHeaders2), RespBody, Req1),

					access_log(Date, Method, Path, 200, IP, ebroker_util:timestamp() - StartTs),

					{ok, Req2, Opts};
				{ok, Req1, {error, Code, Reason}} when is_integer(Code), is_binary(Reason) ->
					Req2 = cowboy_req:reply(Code, #{
						<<"content-type">> => <<"text/plain; charset=utf-8">>
					}, Reason, Req1),

					access_log(Date, Method, Path, Code, IP, ebroker_util:timestamp() - StartTs),

					{ok, Req2, Opts}
			end
	end.

%% 转发请求
forward_request(Req, Location=#location{proxy_pass = Pass, proxy_set_headers = SetHeaders}, Captured) ->
	Method = cowboy_req:method(Req),
	Headers0 = cowboy_req:headers(Req),
	Headers = lists:map(fun({Key, Val}) ->
		Key1 = list_to_binary(string:to_lower(binary_to_list(Key))),
		{Key1, Val}
	                    end, maps:to_list(Headers0)),

	%% 读取请求体
	{ok, Body, Req1} = case cowboy_req:has_body(Req) of
		                   true  -> read_body(Req);
		                   false -> {ok, <<>>, Req}
	                   end,
	%% 获取到请求参数
	Args = binary_to_list(cowboy_req:qs(Req1)),

	%% 处理proxy_pass
	URIMap = uri_string:parse(Pass),
	PoolName0 = maps:get(host, URIMap),
	Path0 = maps:get(path, URIMap),
	QS0 = maps:get(query, URIMap, ""),

	lager:debug("the upstream host is: ~p, query: ~p", [PoolName0, QS0]),

	Path = case QS0 =:= "" of
			   true  -> Path0;
			   false -> Path0 ++ "?" ++  QS0
	       end,

	%% 替换掉相关的参数
	Path1 = replace_args(Captured, Path),
	%% 替换掉$args
	Path2 = lists:flatten(string:replace(Path1, "$args", Args)),

	PoolName = list_to_existing_atom(PoolName0),
	{ok, {Host, Port}} = ebroker_upstreams_ets:get_server(PoolName),
	lager:debug("[http_handler] target host: ~p, port: ~p", [Host, Port]),

	Url = case Port =:= 80 of
		      true ->
			      "http://" ++ Host ++ Path2;
		      false ->
			      "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ Path2
	      end,
	lager:debug("[http_handler] full request url is: ~ts", [Url]),
	Request = #request{
		protocol = http,
		method = Method,
		url = Url,
		headers = merge_headers(Headers, SetHeaders),
		body = Body,
		pool_name = PoolName
	},

	lager:debug("[http_handler] the request is: ~p", [Request]),

	{ok, SchedulerPid} = ebroker_agent:start(self(), Request, Location),
	receive
		{response, SchedulerPid, Response} ->
			{ok, Req1, Response};
		{'EXIT', SchedulerPid, Reason} ->
			lager:error("[http_handler] ebroker_scheduler exit with reason: ~p", [Reason]),
			{ok, Req1, {error, 500, <<"Server Error">>}}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 替换掉相关的参数
replace_args(Args, Path) when is_list(Args) ->
	replace_args0(Args, Path, 1).
replace_args0([], Path, _) ->
	Path;
replace_args0([Arg|Tail], Path, Pos) ->
	Path1 = lists:flatten(string:replace(Path, "$" ++ integer_to_list(Pos), Arg)),
	replace_args0(Tail, Path1, Pos + 1).

%% 合并headers
merge_headers(Headers, []) ->
	Headers;
merge_headers(Headers, SetHeaders) ->
	Keys = proplists:get_keys(SetHeaders),
	Headers2 = lists:foldl(fun(Key, Headers1) -> delete_key(Key, Headers1) end, Headers, Keys),
	Headers2 ++ SetHeaders.

%% 删除相同的key
delete_key(_Key, []) ->
	[];
delete_key(Key, [{Key, _}|Ps]) ->
	delete_key(Key, Ps);
delete_key(Key, [P|Ps]) ->
	[P | delete_key(Key, Ps)].

%% 读取请求体部分
read_body(Req) ->
	read_body(Req, <<>>).
read_body(Req, BodyAcc) ->
	case cowboy_req:read_body(Req) of
		{more, Body, Req1} ->
			read_body(Req1, <<BodyAcc/binary, Body/binary>>);
		{ok, Body, Req1} ->
			{ok, <<BodyAcc/binary, Body/binary>>, Req1}
	end.

%% 访问日志
access_log(Date, Method, Path, StatusCode, IP, CostTs) ->
	Message = lists:flatten(io_lib:format("[~s] ~s | \"~s\" | ~b | \"~s\" | ~.4f(ms)", [
		Date, Method, Path, StatusCode, ip_str(IP), CostTs * 1000
	])),
	lager:info(Message).

ip_str({IP1, IP2, IP3, IP4}) ->
	lists:flatten(io_lib:format("~b.~b.~b.~b", [IP1, IP2, IP3, IP4]));
ip_str(_) ->
	"--".
