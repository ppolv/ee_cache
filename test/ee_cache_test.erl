-module(ee_cache_test).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    timer:sleep(20),
    {ok, _Pid} = ee_cache:start_link(200, 100),
    timer:sleep(20).
cleanup(_) ->
    ee_cache:stop().

ee_cache_test_() ->
        {foreach,
         fun setup/0,
         fun cleanup/1,
        [
            {"Keys are found",
                fun test_keys_found/0
            },
            {"Keys are invalidated",
                fun test_keys_invalidated/0
            },
            {"Keys are evicted by time",
                fun test_keys_timeout/0
            },
            {"Concurrent request make single retrieval",
                fun test_concurrent_request/0
            },
            {"Exceptions are replayed",
                fun test_exceptions/0
            },
            {"Underling retrieval timeouts are handled",
                fun test_timeouts/0
            }

        ]}.

test_keys_found() ->
    ee_cache:put(a, "A"),
    ?assertEqual({ok, "XX"}, ee_cache:get(x, {fun(X) ->X++X end, ["X"]})),
    ?assertEqual({ok,"A"}, ee_cache:get(a, fun() -> "B" end)),
    {ok,B} =  ee_cache:get(b, fun() -> now() end),
    ?assertEqual({ok,B}, ee_cache:get(b, fun() -> now() end)).

test_keys_invalidated() ->
    ee_cache:put(a, "A"),
    {ok,B} =  ee_cache:get(b, fun() -> now() end),
    ee_cache:invalidate(a),
    ee_cache:invalidate(b),
    ?assertEqual({ok,"B"}, ee_cache:get(a, fun()-> "B" end)), %%we must get "B", not "A".
    ?assertNot({ok,B} == ee_cache:get(b, fun() -> now() end)). 

test_keys_timeout() ->
    ee_cache:put(a, "A"),
    {ok,B} =  ee_cache:get(b, fun() -> now() end),
    timer:sleep(300),
    ?assertEqual({ok,"B"}, ee_cache:get(a, fun()-> "B" end)), %%we must get "B", not "A".
    ?assertNot({ok,B} == ee_cache:get(b, fun() -> now() end)). 


collect_all(X) ->
    collect_all(X, []).
collect_all(X, Accum) ->
    receive 
        X ->
            collect_all(X, [X|Accum])
    after 
        110 ->
            lists:reverse(Accum)
    end.


test_concurrent_request() ->
    Self = self(),
    lists:foreach(fun(_) ->
                spawn_link(fun() ->
                            R = ee_cache:get(a, fun() ->
                                        Self ! a,
                                        "A"
                                end),
                            ?assertEqual({ok,"A"}, R)
                    end)
        end, lists:seq(1,1000)),
    lists:foreach(fun(_) ->
                spawn_link(fun() ->
                            R = ee_cache:get(b, fun() ->
                                        Self ! b,
                                        "B"
                                end),
                            ?assertEqual({ok,"B"}, R)
                    end)
        end, lists:seq(1,1000)),
    %% callback must had been called, but only once for each key. Actually it might had been called twice if there
    %% was a cleanup just in the middle, that's ok.  But no more than 2
    As = length(collect_all(a)),
    Bs = length(collect_all(b)),
    ?assert( As =< 2),
    ?assert( As > 0),
    ?assert( Bs =< 2),
    ?assert( Bs > 0),
    timer:sleep(300), 
    %%after cleanup, we continue to work as usual
    lists:foreach(fun(_) ->
                spawn_link(fun() ->
                            R = ee_cache:get(a, fun() ->
                                        Self ! a,
                                        "A"
                                end),
                            ?assertEqual({ok,"A"}, R)
                    end)
        end, lists:seq(1,1000)),
    lists:foreach(fun(_) ->
                spawn_link(fun() ->
                            R = ee_cache:get(b, fun() ->
                                        Self ! b,
                                        "B"
                                end),
                            ?assertEqual({ok,"B"}, R)
                    end)
        end, lists:seq(1,1000)),
    %% callback must had been called, but only once for each key. Actually it might had been called twice if there
    %% was a cleanup just in the middle, that's ok.  But no more than 2
    As2 = length(collect_all(a)),
    Bs2 = length(collect_all(b)),
    ?assert( As2 =< 2),
    ?assert( As2 > 0),
    ?assert( Bs2 =< 2),
    ?assert( Bs2 > 0).





test_exceptions() ->
    ?assertEqual({'error', {'throw', error}}, ee_cache:get(a, fun() -> throw(error) end)),
    ?assertEqual({'error', {'exit', error}}, ee_cache:get(b, fun() -> exit(error) end)),
    {'error', {'throw', N}} =  ee_cache:get(c, fun() -> throw(now()) end),
    ?assertEqual({error, {throw, N}}, ee_cache:get(c, fun() -> throw(now()) end)), %%we get the cached error
    ?assertEqual({error, {throw, N}}, ee_cache:get(c, fun() -> throw(now()) end)). %%we get the cached error

test_timeouts() ->
    Parent = self(),
    Key = key,
    lists:foreach(fun(_) ->
        spawn_link(fun() ->
            ?assertEqual({error, {throw, {'no_response_from_retrieval_fun', Key}}},
                 ee_cache:get(Key, 
                    fun() -> Parent ! callback, timer:sleep(150), "A" end)),
            Parent ! done
            end) 
            end, 
            lists:seq(1,1000)
        ),
    Callbacks = length(collect_all(callback)),
    ?assert(Callbacks > 0),
    ?assert(Callbacks =< 2),
    ?assertEqual(1000, length(collect_all(done))).




