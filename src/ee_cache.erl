-module(ee_cache).
%% ee_cache  Extremely simple, in-memory cache.
%% Cache entries for up-to  N seconds.
%% 
%%

-define(TABLE, ee_cache).
-define(WAITING_TABLE, ee_blocked).
-record(state, {
        table, 
        waiting_table,   %%blocked request, waiting for underling response from db
        clean_interval,
        retrieval_timeout  %%how much to wait before given up in a underling request
    }).

%% API
-export([start_link/1, start_link/2, get/2, put/2, invalidate/1, stop/0]).

%% gen_server
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_RETRIEVAL_TIMEOUT, 10000).
-define(DEFAULT_POPULATE_TIMEOUT, 12000).  
    %% this make the gen_server to return an error instead if the exit.  Should never happens anyway

start_link(CleanInterval) ->
    start_link(CleanInterval, ?DEFAULT_RETRIEVAL_TIMEOUT).
start_link(CleanInterval, RetrievalTimeout) ->
	gen_server:start_link({local,?MODULE},?MODULE, [CleanInterval,RetrievalTimeout], []).

invalidate(Key) ->
    ets:delete(?TABLE, Key).

get(Key, Fun) ->
	case ets:lookup(?TABLE, Key) of
        [{Key, Value}] ->
            Value;
        [] ->  
            gen_server:call(?MODULE, {populate, Key, Fun}, ?DEFAULT_POPULATE_TIMEOUT)
	end.

put(Key, Value) ->
	ets:insert(?TABLE, {Key, {ok,Value}}).

stop() ->
    gen_server:call(?MODULE, stop).

init([CleanIntervalMilliSec, RetrievalTimeout]) ->
	Table = ets:new(?TABLE, [set, named_table, public]),  %%TODO: {read_concurrency, true}
	WaitingTable = ets:new(?WAITING_TABLE, [set]),  
    {ok, _TRef} = timer:send_interval(CleanIntervalMilliSec, self(), clean_cache_timeout),
	{ok, #state{table = Table, 
            waiting_table = WaitingTable, 
            clean_interval = CleanIntervalMilliSec,
            retrieval_timeout = RetrievalTimeout
        }}.
handle_call({populate, Key, Fun}, From, State = #state{waiting_table = WT}) ->
    case ets:lookup(WT, Key) of
        [{Key, TRef, List}] ->
            ets:insert(WT, {Key, TRef, [From|List]});
        [] ->
            %%In case the spawned fun did not answer in 10 seconds
            {ok, TRef} = timer:send_after(
                            State#state.retrieval_timeout, self(), {retrieval_timeout, Key}),  
            Server = self(),
            spawn(fun() ->
                        try gen_server:cast(Server,{result, TRef, Key, {ok,retrieve(Fun)}})
                        catch
                            Type:Error ->
                                gen_server:cast(Server,{result, TRef, Key, {error, {Type, Error}}})
                        end
                end),
            ets:insert(WT, {Key, TRef, [From]})
    end,
    {noreply, State};

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(_Call, _From, State) ->
	{reply, ok, State}.
handle_cast({result, TRef, Key, Result}, State = #state{table = T, waiting_table = WT}) ->
    case ets:lookup(WT, Key) of
        [{Key, TRef, List}] ->
            timer:cancel(TRef),
            lists:foreach(fun(Blocked) ->
                        gen_server:reply(Blocked, Result)
                end, List),
            ets:delete(WT, Key),
            ets:insert(T, {Key, Result});
        _ ->
            ok  %% otherwise do nothing. 
    end,
	{noreply, State}.
handle_info({retrieval_timeout, Key}, State = #state{waiting_table = WT}) ->
    %% The db retrieval failed to complete in 10 seconds, or the retrieval process died.
    %% here we return timeout to all blocked process.  Note this shouldn't happens as DB timeouts,
    %% gen_servers timeouts are 5 seconds, so things should had been detected before. 
    case ets:lookup(WT, Key) of
        [{Key, _TRef, List}] ->
            lists:foreach(fun(Blocked) ->
                        gen_server:reply(Blocked, {error, {throw, {'no_response_from_retrieval_fun', Key}}})
                end, List),
            ets:delete(WT, Key);
        _ ->
            ok  %% otherwise do nothing. 
    end,
	{noreply, State};
handle_info(clean_cache_timeout, #state{table = Table} = State) ->
	ets:delete_all_objects(Table),
	{noreply, State}.
terminate(_Reason, _State) ->
	ok.
code_change(_OldVs, State, _NewVsn) ->
	{ok, State}.



retrieve({Fun,Args}) when is_function(Fun) ->
    apply(Fun, Args);
retrieve(Fun) when is_function(Fun) ->
    Fun().
