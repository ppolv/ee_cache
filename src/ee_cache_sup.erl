-module(ee_cache_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, [EvictEach]}, permanent, 5000, Type, [I]}).

start_link(EvictEach) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [EvictEach]).

init([EvictEach]) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(ee_cache, worker)]}}.
	
