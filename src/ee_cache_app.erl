-module(ee_cache_app).

-behaviour(application).



%% Application callbacks
-export([start/2, stop/1]).

 %% ===================================================================
 %% Application callbacks
 %% ===================================================================

start(_StartType, _StartArgs) ->
	EvictEach = application:get_env(ee_cache, evict_each, 10), %%default is evict each 10 seconds
	ee_cache_sup:start_link(EvictEach).

stop(_State) ->
	ok.
