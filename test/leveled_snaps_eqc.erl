%%% @author Thomas Arts <thomas@SpaceGrey.lan>
%%% @copyright (C) 2019, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created :  4 Mar 2019 by Thomas Arts <thomas@SpaceGrey.lan>

-module(leveled_snaps_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-eqc_group_commands(false).

-compile([export_all, nowarn_export_all]).

-define(MODEL, leveledjc_eqc).
-define(sut_names, [first, second, third, fourth, fifth, sixth, seventh]).

%% Don't put sequential => true in initial map, since we run each snapshot sequential but
%% don't yet want to update load on a snapshot
initial_state() ->
    [{sut, leveledjc_eqc:initial_state(sut)}].

command(S) ->
    ?LET({Name, LS}, elements(S),
    frequency([{40,
                ?LET({call, _Model, F, Args}, ?MODEL:command(LS),
                     {call, ?MODULE, cmd, [F, Name, Args]})},
                {case maps:get(leveled, LS) of undefined -> 0; _ -> 1 end,
                 ?LAZY({call, ?MODULE, snapshot, [lists:nth(length(S), ?sut_names), Name,
                                                  maps:get(tag, LS),  [{snapshot_bookie, maps:get(leveled, LS)} |
                                                                 maps:get(start_opts, LS)]]})
                }
              ])).

precondition(S, {call, _, snapshot, [_SnapName, Original, Tag, Opts]}) ->
    case proplists:get_value(Original, S) of
        undefined -> false;
        LS ->
            Tag == maps:get(tag, LS) andalso
                tl(Opts) == maps:get(start_opts, LS)
    end;
precondition(S, {call, _, cmd, [F, Name, Args]}) ->
    case proplists:get_value(Name, S) of
        undefined -> false;
        LS ->
            ?MODEL:precondition(LS, {call, ?MODEL, F, Args})
    end.

adapt(S, {call, _, cmd, [F, Name, Args]}) ->
    case proplists:get_value(Name, S) of
        undefined -> false;
        LS ->
            case ?MODEL:adapt(LS, {call, ?MODEL, F, Args}) of
                false -> false;
                NewArgs ->
                    {call, ?MODULE, cmd, [F, Name, NewArgs]}
            end
    end;
adapt(S, {call, _, snapshot, [SnapName, Original, _Tag, _Opts]}) ->
    case proplists:get_value(Original, S) of
        undefined -> false;
        LS ->
          {call, ?MODULE, [SnapName, Original,
                           maps:get(tag, LS),
                           [{snapshot_bookie, maps:get(leveled, LS)} | maps:get(start_opts, LS)]]}
    end.

cmd(F, _Name, Args) ->
    apply(?MODEL, F, Args).

snapshot(Name, Original, Tag, Opts) ->
    io:format("snapshotting ~p\n", [Original]),
    ?MODEL:init_backend(Tag, lists:keydelete(root_path, 1, Opts), Name).


next_state(S, V, {call, _, cmd, [F, Name, Args]}) ->
    LS = proplists:get_value(Name, S),
    NewLS = ?MODEL:next_state(LS, V, {call, ?MODEL, F, Args}),
    lists:keyreplace(Name, 1, S, {Name, NewLS});
next_state(S, SnapShotPid, {call, _, snapshot, [Name, Original, _Tag, _Opts]}) ->
    LS = proplists:get_value(Original, S),
    [{Name, LS#{sut => Name, leveled => SnapShotPid}} | S].


postcondition(S, {call, _, cmd, [F, Name, Args]}, Res) ->
    LS = proplists:get_value(Name, S),
    ?MODEL:postcondition(LS, {call, ?MODEL, F, Args}, Res);
postcondition(_S, {call, _, snapshot, _}, Res) ->
    is_pid(Res).


call_features(S, {call, _, cmd, [F, Name, Args]}, Res) ->
    LS = proplists:get_value(Name, S),
    ?MODEL:call_features(LS, {call, ?MODEL, F, Args}, Res);
call_features(_S, {call, _, snapshot, [Name, _, _, _Opts]}, _Res) ->
    [{snapshot, Name}].


prop_db() ->
    Dir = "./leveled_data",
    eqc:dont_print_counterexample(
    ?LET(Shrinking, parameter(shrinking, false),
    ?FORALL(Cmds, commands(?MODULE, initial_state()),
    begin
        ?MODEL:delete_level_data(Dir),
        ?IMPLIES(empty_dir(Dir),
        ?ALWAYS(if Shrinking -> 40; true -> 1 end,
        begin
            Procs = erlang:processes(),
            StartTime = erlang:system_time(millisecond),

            RunResult = run_commands(Cmds, [{dir, Dir}]),
            %% Do not extract the 'state' from this tuple, since parallel commands
            %% miss the notion of final state.
            CallFeatures = [ Feature || Feature <- call_features(history(RunResult)),
                                        not is_foldaccT(Feature),
                                        not (is_tuple(Feature) andalso element(1, Feature) == start_options)
                           ],
            StartOptionFeatures = [ lists:keydelete(root_path, 1, Feature) || {start_options, Feature} <- call_features(history(RunResult)) ],

            [ case whereis(N) of
                  undefined ->
                      ?MODEL:delete_level_data(Dir);
                  Pid when is_pid(Pid) ->
                      leveled_bookie:book_destroy(Pid)
              end || N <- [sut | ?sut_names]],

            Wait = ?MODEL:wait_for_procs(Procs, 12000),
                % Wait at least for delete_pending timeout + 1s
                % However, even then can hit the issue of the Quviq license
                % call spawning processes
            lists:foreach(fun(P) ->
                                io:format("~nProcess info for ~w:~n~w~n",
                                            [P, process_info(P)]),
                                io:format("Stacktrace:~n ~w~n",
                                            [process_info(P, current_stacktrace)]),
                                io:format("Monitored by:~n ~w~n",
                                            [process_info(P, monitored_by)]),
                                io:format("~n")
                            end,
                            Wait),
            RunTime = erlang:system_time(millisecond) - StartTime,

            %% Since in parallel commands we don't have access to the state, we retrieve functions
            %% from the features
            FoldAccTs = [ FoldAccT || Entry <- history(RunResult),
                                      {foldAccT, FoldAccT} <- eqc_statem:history_features(Entry)],

            pretty_commands(?MODULE, Cmds, RunResult,
            measure(time_per_test, RunTime,
            aggregate(with_title('Features'), CallFeatures,
            aggregate(with_title('Start Options'), StartOptionFeatures,
            features(CallFeatures,
                      conjunction([{result,
                                    ?WHENFAIL([ begin
                                                    eqc:format("~p with acc ~p:\n~s\n", [F, Acc,
                                                                                         show_function(F)])
                                                end || {F, Acc} <- FoldAccTs ],
                                              result(RunResult) == ok)},
                                   {data_cleanup,
                                    ?WHENFAIL(eqc:format("~s\n", [os:cmd("ls -Rl " ++ Dir)]),
                                              empty_dir(Dir))},
                                   {pid_cleanup, equals(Wait, [])}]))))))
        end))
    end))).

history({H, _, _}) -> H.
result({_, _, Res}) -> Res.

is_exit({'EXIT', _}) ->
    true;
is_exit(Other) ->
    {expected_exit, Other}.

is_foldaccT({foldAccT, _}) ->
    true;
is_foldaccT(_) ->
    false.

show_function(F) ->
    case proplists:get_value(module, erlang:fun_info(F)) of
        eqc_fun ->
            eqc_fun:show_function(F);
        _ ->
            proplists:get_value(name, erlang:fun_info(F))
    end.

empty_dir(Dir) ->
    case file:list_dir(Dir) of
        {error, enoent} -> true;
        {ok, Ds} ->
            lists:all(fun(D) -> empty_dir(filename:join(Dir, D)) end, Ds);
        _ ->
            false
    end.
