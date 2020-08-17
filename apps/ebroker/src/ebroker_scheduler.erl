%%%-------------------------------------------------------------------
%%% @author licheng5
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 7月 2020 4:43 下午
%%%-------------------------------------------------------------------
-module(ebroker_scheduler).
-author("licheng5").
-include("ebroker.hrl").

-behaviour(gen_server).

%% API
-export([start/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	receiver_pid,
	location,
	request,
	tasks = [],
	task_num = 0,
	response_num = 0,
	timer_ref,
	is_replied = false,

	%% 时间统计
	start_ts = 0,
	end_ts = 0
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(ReceiverPid :: pid(), Req::#request{}, Location::#location{}) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start(ReceiverPid, Request=#request{}, Location=#location{}) ->
	gen_server:start(?MODULE, [ReceiverPid, Request, Location], []).

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
init([ReceiverPid, Request, Location=#location{proxy_read_timeout = ReadTimeout, proxy_retry_timeout = RetryTimeout}]) ->
	erlang:process_flag(trap_exit, true),

	erlang:start_timer(ReadTimeout, self(), read_timeout),
	TimerRef = erlang:start_timer(RetryTimeout, self(), retry_ticker),
	%% 执行任务
	{ok, TaskPid} = http_task:start_link(),
	http_task:async_request(TaskPid, self(), Request),

	{ok, #state{receiver_pid = ReceiverPid, request = Request, location = Location,
		timer_ref = TimerRef, tasks = [TaskPid], task_num = 1, start_ts = ebroker_util:timestamp()}}.

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
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

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
handle_info({timeout, _, retry_ticker}, State=#state{is_replied = IsReplied, task_num = TaskNum, tasks = Tasks, request = Request}) ->
	if
		IsReplied =:= true ->
			{noreply, State};
		IsReplied =:= false andalso TaskNum =:= 1 ->
			lager:debug("[ebroker_scheduler] retry_ticker start new task"),
			%% 重新执行任务
			{ok, TaskPid} = http_task:start_link(),
			http_task:async_request(TaskPid, self(), Request),
			{noreply, State#state{tasks = [TaskPid|Tasks], task_num = TaskNum + 1}};
		true ->
			{noreply, State}
	end;

%% 达到最大的请求时间
handle_info({timeout, _, read_timeout}, State=#state{receiver_pid = ReceiverPid, is_replied = IsReplied}) ->
	case IsReplied of
		true ->
			{noreply, State};
		false ->
			ReceiverPid ! {response, self(), {error, 504, <<"Gateway Read Timeout">>}},
			{noreply, State#state{is_replied = true, end_ts = ebroker_util:timestamp()}}
	end;

handle_info({async_response, {ok, _, _, _}=Response}, State=#state{receiver_pid = ReceiverPid, response_num = ResponseNum, is_replied = false}) ->
	%% 返回请求结果
	ReceiverPid ! {response, self(), Response},
	{noreply, State#state{is_replied = true, response_num = ResponseNum + 1, end_ts = ebroker_util:timestamp()}};
%% 未达到重试的阈值，并且是第一个请求的响应
handle_info({async_response, {error, _, _}=Response}, State=#state{timer_ref = TimerRef, request = Request, response_num = ResponseNum, is_replied = false,
	tasks = Tasks, task_num = TaskNum, receiver_pid = ReceiverPid}) ->
	if
		TaskNum =:= 1 ->
			lager:debug("[ebroker_scheduler] start new task because first task return a error"),
			erlang:cancel_timer(TimerRef),
			{ok, TaskPid} = http_task:start_link(),
			http_task:async_request(TaskPid, self(), Request),
			{noreply, State#state{response_num = ResponseNum + 1, tasks = [TaskPid|Tasks], task_num = TaskNum + 1}};
		TaskNum =:= 2 andalso ResponseNum =:= 0 ->
			lager:debug("[ebroker_scheduler] ignore first return"),
			{noreply, State#state{response_num = ResponseNum + 1}};
		true ->
			%% 返回请求结果
			lager:debug("[ebroker_scheduler] will force return a result"),
			ReceiverPid ! {response, self(), Response},
			{noreply, State#state{is_replied = true, response_num = ResponseNum + 1, end_ts = ebroker_util:timestamp()}}
	end;

handle_info({async_response, Response}, State) ->
	lager:debug("[ebroker_scheduler] get a respsone: ~p", [Response]),
	{noreply, State};

%% 注意子任务的退出
handle_info({'EXIT', Pid, Reason}, State=#state{tasks = Tasks}) ->
	lager:debug("the task exit with reason: ~p", [Reason]),
	Tasks1 = lists:delete(Pid, Tasks),
	case Tasks1 =:= [] of
		true ->
			{stop, normal, State};
		false ->
			{noreply, State#state{tasks = Tasks1}}
	end.

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
terminate(Reason, #state{start_ts = StartTs, end_ts = EndTs, task_num = TaskNum, request = #request{url = Url, pool_name = PoolName}}) ->
	lager:info("[ebroker_scheduler] exit with reason: ~p, url: ~p, cost time: ~p(ms), pool stats: ~p,  task_num: ~p",
		[Reason, Url, (EndTs - StartTs) * 1000, hackney_pool:get_stats(PoolName), TaskNum]),
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
