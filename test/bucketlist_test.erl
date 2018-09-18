-module(bucketlist_test).

-export([first_of_all/0, map_to_int/0]).

%% This tests demonstrates that the first bucket one gets returned is the same as the 
%% first bucket in the list of returning them all.
%% It seems that buckets are ordered alphabethically in returned search.
first_of_all() ->
    {ok, Pid} = leveled_bookie:book_start([{root_path, "first_of_all"}]),

    {async, FoldFirst} = 
        leveled_bookie:book_bucketlist(Pid, o, 
                                       {fun(B, Acc) -> [B | Acc] end, []}, 
                                       first),
    {async, FoldAll} = 
        leveled_bookie:book_bucketlist(Pid, o, 
                                       {fun(B, Acc) -> [B | Acc] end, []}, 
                                       all),

    leveled_bookie:book_put(Pid, <<"bucket2">>, 
                            <<2:128>>, <<11:256>>, []),
    leveled_bookie:book_put(Pid, <<"bucket1">>, 
                            <<1:128>>, <<12:256>>, []),
    All = FoldAll(),
    [First] = FoldFirst(),
    io:format("First ~p\nAll ~p\n", [First, All]),
    leveled_bookie:book_destroy(Pid),
    os:cmd("rm -r first_of_all"),
    First = hd(All).

map_to_int() ->
    {ok, Pid} = leveled_bookie:book_start([{root_path, "first_of_all"}]),

    {async, FoldFirst} = 
        leveled_bookie:book_bucketlist(Pid, o, 
                                       {fun(<<"bucket2">>, Acc) -> Acc ++ [0];
                                           (<<"bucket1">>, Acc) -> Acc ++ [1]
                                        end, []}, 
                                       first),
    {async, FoldAll} = 
        leveled_bookie:book_bucketlist(Pid, o, 
                                       {fun(<<"bucket2">>, Acc) -> Acc ++ [0];
                                           (<<"bucket1">>, Acc) -> Acc ++ [1]
                                        end, []}, 
                                       all),

    leveled_bookie:book_put(Pid, <<"bucket2">>, 
                            <<2:128>>, <<11:256>>, []),
    leveled_bookie:book_put(Pid, <<"bucket1">>, 
                            <<1:128>>, <<12:256>>, []),
    All = FoldAll(),
    [First] = FoldFirst(),
    io:format("First ~p\nAll ~p\n", [First, All]),
    leveled_bookie:book_destroy(Pid),
    os:cmd("rm -r first_of_all"),
    First = hd(All).
