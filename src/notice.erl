-module(notice).
-behaviour(gen_server).
-include("ca.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([
    create_notice/3
]).
-define(SERVER, ?MODULE).
-define(TIMEVALE, 3000).
-define(NOTICE_TABLE, notice_table).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

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
    ets:new(?NOTICE_TABLE, [
        set,
        public,
        named_table,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),
    erlang:send_after(?TIMEVALE, self(), notice_send),
    {ok, #state{}}.

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
handle_cast({notice, LogLevel, KvList, Message}, State) ->
    case ?NT of
        true ->
            Reason = proplists:get_value(<<"reason">>, KvList, <<>>),
            TargetUrl = proplists:get_value(<<"url">>, KvList, <<>>),
            TenantId = proplists:get_value(<<"tenant">>, KvList, <<>>),
            Level = ?NT_LEVEL(LogLevel),
            NewContext = list_to_binary(io_lib:format("~p", [Message])),
            NewReason = list_to_binary(io_lib:format("~p", [Reason])),
            notice_event(NewContext, NewReason, TargetUrl, TenantId, Level);
        false ->
            skip
    end,
    {noreply, State};
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
handle_info(notice_send, State) ->
    notice(),
    erlang:send_after(?TIMEVALE, self(), notice_send),
    {noreply, State};
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

create_notice(LogLevel, Kvlist, Message) ->
    gen_server:cast(?SERVER, {notice, LogLevel, Kvlist, Message}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
notice() ->
    List = ets:tab2list(?NOTICE_TABLE),
    Fun = fun(K, V) ->
        notice_send(V, 2),
        ets:delete(?NOTICE_TABLE, K)
          end,
    [Fun(K, V) || {K, V} <- List].

notice_send(_Body, 0) ->
    ok;
notice_send(Body, Retry) ->
    Url = application:get_env(?APP_NAME, notice_url, "http://nt.csp.test.sankuai.com/v1/push_notice"),
    case httpc:request(post, {Url, [], "application/json", Body}, [{timeout, ?NT_TIMEOUT}], [{body_format, binary}]) of
        {ok, {{_Version,200, _Msg},_Server, ResBody}} ->
            lager:info("notice url:~p, reqbody:~p, result:~p", [Url, Body, ResBody]);
        {ok, Result}->
            lager:error("notice url:~p, reqbody:~p, result:~p", [Url, Body, Result]),
            notice_send(Body, Retry - 1);
        {error, Error} ->
            lager:error("notice url:~p, reqbody:~p, error:~p", [Url, Body, Error]),
            notice_send(Body, Retry - 1)
    end.

notice_event(Context, Reason, TargetUrl, TenantId, Level) ->
    HostName = case application:get_env(?APP_NAME, hostname, undefined) of
                   undefined ->
                       {ok, Host} = inet:gethostname(),
                       BHost = list_to_binary(Host),
                       application:set_env(?APP_NAME, hostname, BHost),
                       BHost;
                   HName ->
                       HName
               end,

    ServerName = application:get_env(?APP_NAME, service_name, atom_to_binary(?APP_NAME, utf8)),

    case ets:lookup(?NOTICE_TABLE, Reason) of
        [] ->
            ets:insert(?NOTICE_TABLE, {Reason, jsx:encode([
                {<<"hostName">>, HostName},
                {<<"level">>, Level},
                {<<"msg">>, Context},
                {<<"reason">>, Reason},
                {<<"serverName">>, ServerName},
                {<<"targetUrl">>, TargetUrl},
                {<<"tenantId">>, TenantId}
            ])});
        _V ->
            ok
    end.

