-module(ee_cache).
%% ee_cache  Extremely simple, in-memory cache.
%% Cache entries for up-to  N seconds.
%% 
%%

-define(TABLE, ee_cache).
-record(state, {table, clean_interval}).

%% API
-export([start_link/1, get/1, put/2]).

%% gen_server
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).


start_link(CleanInterval) ->
	gen_server:start_link(?MODULE, CleanInterval, []).

get(Key) ->
	try {ok, ets:lookup_element(?TABLE, Key, 2)} 
	catch error:badarg ->
		 not_found
	end.

put(Key, Value) ->
	ets:insert(?TABLE, {Key, Value}).

init(CleanIntervalSec) ->
	CleanInterval = CleanIntervalSec * 1000,
	Table = ets:new(?TABLE, [set, named_table, public]),  %%TODO: {read_concurrency, true}
	{ok, #state{table = Table, clean_interval = CleanInterval}, CleanInterval}.
handle_call(_Call, _From, State) ->
	{reply, ok, State}.
handle_cast(_Cast, State) ->
	{noreply, State}.
handle_info(timeout, #state{table = Table, clean_interval = CleanInterval} = State) ->
	ets:delete_all_objects(Table),
	{noreply, State, CleanInterval}.
terminate(_Reason, _State) ->
	ok.
code_change(_OldVs, State, _NewVsn) ->
	{ok, State}.



