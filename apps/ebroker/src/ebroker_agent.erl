%%%-------------------------------------------------------------------
%%% @author xudong12
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 8月 2020 2:31 下午
%%%-------------------------------------------------------------------
-module(ebroker_agent).
-author("xudong12").
-include("ebroker.hrl").

-behaviour(gen_statem).

%% API
-export([start/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3, code_change/4, callback_mode/0]).
-export([request_sended/3, retry/3]).

-record(state, {
	receiver_pid,
	location,
	request,
	tasks = [],
	task_num = 0,
	timer_ref,
	is_replied = false,
	can_skip = true,
	%% 时间统计
	start_ts = 0,
	end_ts = 0
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(ReceiverPid, Request=#request{}, Location=#location{}) ->
	gen_statem:start(?MODULE, [ReceiverPid, Request, Location], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([ReceiverPid, Request, Location=#location{proxy_read_timeout = ReadTimeout, proxy_retry_timeout = RetryTimeout}]) ->
	erlang:process_flag(trap_exit, true),

	erlang:start_timer(ReadTimeout, self(), read_timeout),
	TimerRef = erlang:start_timer(RetryTimeout, self(), retry_ticker),
	%% 执行任务
	{ok, TaskPid} = http_task:start_link(),
	http_task:async_request(TaskPid, self(), Request),

	{ok, request_sended, #state{receiver_pid = ReceiverPid, request = Request, location = Location,
		timer_ref = TimerRef, task_num = 1, tasks = [TaskPid], start_ts = ebroker_util:timestamp()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out 
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
	state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
	Status = some_term,
	Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
request_sended(info, {async_response, {ok, _, _, _}=Response}, State=#state{receiver_pid = ReceiverPid}) ->
	lager:debug("request sended state, get a response: ~p", [Response]),
	ReceiverPid ! {response, self(), Response},
	{stop, normal, State#state{is_replied = true, end_ts = ebroker_util:timestamp()}};
request_sended(info, {async_response, {error, _, _}=Response}, State=#state{timer_ref = TimerRef, request = Request, tasks = Tasks}) ->
	lager:debug("[ebroker_scheduler] start new task because first task return a error, response: ~p", [Response]),
	erlang:cancel_timer(TimerRef),
	{ok, TaskPid} = http_task:start_link(),
	http_task:async_request(TaskPid, self(), Request),
	{next_state, retry, State#state{tasks = [TaskPid | Tasks], can_skip = false, task_num = 2}};
request_sended(info, {timeout, _, retry_ticker}, State=#state{request = Request, tasks = Tasks}) ->
	lager:debug("[ebroker_scheduler] retry_ticker start new task"),
	%% 重新执行任务
	{ok, TaskPid} = http_task:start_link(),
	http_task:async_request(TaskPid, self(), Request),
	{next_state, retry, State#state{tasks = [TaskPid | Tasks], can_skip = true, task_num = 2}};
request_sended(info, Info, State) ->
	handle_info(Info, State).

%% 重试状态下
retry(info, {async_response, {ok, _, _, _}=Response}, State=#state{receiver_pid = ReceiverPid}) ->
	lager:debug("[ebroker_agent] retry state, get a response: ~p", [Response]),
	ReceiverPid ! {response, self(), Response},
	{stop, normal, State#state{is_replied = true, end_ts = ebroker_util:timestamp()}};
retry(info, {async_response, {error, _, _}=Response}, State=#state{can_skip = true}) ->
	lager:debug("[ebroker_agetn] retry state, get a response: ~p", [Response]),
	{keep_state, State#state{can_skip = false}};
retry(info, {async_response, Response}, State=#state{receiver_pid = ReceiverPid, can_skip = false}) ->
	ReceiverPid ! {response, self(), Response},
	{stop, normal, State#state{is_replied = true, end_ts = ebroker_util:timestamp()}};
retry(info, {timeout, _, retry_ticker}, State) ->
	{keep_state, State};
retry(info, Info, State) ->
	handle_info(Info, State).

%% 处理公共部分的消息
handle_info({timeout, _, read_timeout}, State=#state{receiver_pid = ReceiverPid}) ->
	ReceiverPid ! {response, self(), {error, 504, <<"Gateway Read Timeout">>}},
	{stop, normal, State#state{is_replied = true, end_ts = ebroker_util:timestamp()}};
handle_info({'EXIT', Pid, Reason}, State=#state{tasks = Tasks}) ->
	lager:debug("[ebroker_agent] the task exit with reason: ~p", [Reason]),
	Tasks1 = lists:delete(Pid, Tasks),
	case Tasks1 =:= [] of
		true ->
			{stop, normal, State};
		false ->
			{keep_state, State#state{tasks = Tasks1}}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, StateName, #state{start_ts = StartTs, end_ts = EndTs, task_num = TaskNum, is_replied = IsReplied, receiver_pid = ReceiverPid, request = #request{url = Url, pool_name = PoolName}}) ->
	case IsReplied of
		true ->
			ok;
		false ->
			ReceiverPid ! {response, self(), {error, 504, <<"Gateway Read Timeout">>}}
	end,
	lager:info("[ebroker_agent] ternimate(state: ~p) with reason: ~p, url: ~p, cost time: ~p(ms), pool stats: ~p,  task_num: ~p",
		[StateName, Reason, Url, (EndTs - StartTs) * 1000, hackney_pool:get_stats(PoolName), TaskNum]),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
