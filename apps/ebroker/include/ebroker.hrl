%%%-------------------------------------------------------------------
%%% @author xudong12
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 7月 2020 2:32 下午
%%%-------------------------------------------------------------------
-author("xudong12").

-record(location, {
	url_regexp :: string(),
	url_regexp_mp,
	proxy_pass :: string(),
	proxy_http_version :: float(),
	proxy_set_headers = [] :: [{string(), string()}],
	proxy_connect_timeout :: integer(),
	proxy_send_timeout :: integer(),
	proxy_read_timeout :: integer(),
	proxy_retry_timeout :: integer()
}).

-record(upstream, {

}).

%% 请求
-record(request, {
	protocol,
	method,
	url,
	headers = [],
	body,
	pool_name
}).