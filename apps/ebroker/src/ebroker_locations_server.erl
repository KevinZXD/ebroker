%%%-------------------------------------------------------------------
%%% @author licheng5
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 7月 2020 2:37 下午
%%%-------------------------------------------------------------------
-module(ebroker_locations_server).
-author("licheng5").
-include("ebroker.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_locations/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	locations = []
}).

%%%===================================================================
%%% API
%%%===================================================================

get_locations() ->
	gen_server:call(?SERVER, get_locations).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok, Locations} = application:get_env(ebroker, locations),
	lager:info("locations: ~p", [Locations]),
	%% 编译全部的locations
	Locations1 = lists:map(fun(Location) -> compile_location(Location) end, Locations),
	{ok, #state{locations = Locations1}}.

compile_location({UrlRegexp, Opts}) ->
	{ok, MP} = re:compile(UrlRegexp),
	Location = #location{url_regexp = UrlRegexp, url_regexp_mp = MP},
	parse_opts(Opts, Location).

parse_opts([], Location) ->
	Location;
parse_opts([{proxy_pass, ProxyPass}|Tail], Location) ->
	parse_opts(Tail, Location#location{proxy_pass = ProxyPass});
parse_opts([{proxy_http_version, HttpVer}|Tail], Location) ->
	parse_opts(Tail, Location#location{proxy_http_version = HttpVer});
parse_opts([{proxy_set_header, Key, Value}|Tail], Location=#location{proxy_set_headers = Headers}) ->
	parse_opts(Tail, Location#location{proxy_set_headers = [{list_to_binary(string:to_lower(Key)), list_to_binary(Value)}|Headers]});
parse_opts([{proxy_connect_timeout, ConnectTimeout}|Tail], Location) ->
	parse_opts(Tail, Location#location{proxy_connect_timeout = ConnectTimeout});
parse_opts([{proxy_send_timeout, SendTimeout}|Tail], Location) ->
	parse_opts(Tail, Location#location{proxy_send_timeout = SendTimeout});
parse_opts([{proxy_read_timeout, ReadTimeout}|Tail], Location) ->
	parse_opts(Tail, Location#location{proxy_read_timeout = ReadTimeout});
parse_opts([{proxy_retry_timeout, RetryTimeout}|Tail], Location) ->
	parse_opts(Tail, Location#location{proxy_retry_timeout = RetryTimeout});
parse_opts([_|Tail], Location) ->
	parse_opts(Tail, Location).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call(get_locations, _From, State=#state{locations = Locations}) ->
	{reply, {ok, Locations}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
