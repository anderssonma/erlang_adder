%% @doc Erlang mini project.
-module(add).
-export([m/0, start/3, start/4, split/2, padd/2]).

%Work?

m() -> start(12345, 6789, 10).

split(L, N) when length(L) < N ->
    L;

%% Do the splitting
split(L, N) ->
    split(L, N, []).

%% An auxiliary recursive split function
split(L, N, Lists) ->
    {L1, L2} = lists:split(N, L),
    if length(L2) > N ->
	    split(L2, N, [L1|Lists]);
       true ->
	    [L2, L1|Lists]
    end.


sep(0, Acc) -> Acc;
sep(N, Acc) ->
    Next = N rem 10,
    %%io:format("N ~p~n", [N]),
    %%io:format("Next ~p~n", [Next]),
    sep(trunc(N / 10), [Next | Acc]).

sep(N) -> sep(N, []).


padd(A, B) when length(A) =:= length(B) ->
    {A, B};
padd(A, B) when length(A) >= length(B) ->
    padd(A, [0 | B]);
padd(A, B) ->
    padd([0 | A], B).

%% divisible(M, N) when M / N =:= 0 ->
%%     M / N;
%% divisible(M, N) ->
%%     N + 1.


worker([H | []], Ppid, Count) -> 
    {Cout, Sum} = calc(H, 0, 0),
    
    Ppid ! {sum, Sum rem 10, Count},
    exit(Cout);

worker([Head | Pairs], Ppid, Count) ->
    
    {Cout0, Sum0} = calc(Head, 0, 0),
    {Cout1, Sum1} = calc(Head, 1, 0),
    
    process_flag(trap_exit,true),
    spawn_link(fun() -> worker(Pairs, Ppid,Count+1) end),

    receive 
        {'EXIT', _PID, no_carry} ->
            Ppid ! {sum, Sum0 rem 10, Count},
            exit(Cout0);
        {'EXIT', _PID, carry} ->
            Ppid ! {sum, Sum1 rem 10, Count},
            exit(Cout1)
    end.



calc([], Cout,Sum) ->    
    case Cout =:= 1 of 
        true ->
            {carry, Sum};
        false ->
            {no_carry, Sum}
    end;
    
calc([{A, B} | T], Cin,Sum) ->
    Tot = A + B + Cin,
    case Tot >= 10 of
        true ->
            calc(T, 1, Tot + Sum);
        false ->
            calc(T, 0, Tot + Sum)
    end.

%% Get correcgt start divider
divider(Sum,0)-> Sum;
divider(Sum,Count)->
    divider(Sum*10,Count-1).

%% Concat sorted list into correct sum
concat(List) ->
    Divider = divider(1,length(List)-1),
    io:format("Divider size : ~p~n", [Divider]),
    concat(List,Divider,0).
 
concat([], _Divider, Sum) ->
    Sum;
    
concat([H | List], Divider, Sum) ->
    concat(List,trunc(Divider/10), Sum + H*Divider).

%% Sort the received sums in correct order
sort(Map,List,0) -> 
    NewList = [maps:get(0, Map) | List],
    io:format("Sort : ~p~n", [NewList]),
    NewList;
sort(Map,List,Pos) ->
    sort(Map,[maps:get(Pos, Map) | List],Pos-1).
    
    
loop(0, List) -> 
    io:format("loop Innan Map : ~p~n", [List]),
    Map = maps:from_list(List),
    %%io:format("loop Map : ~p~n", [Map]),
    Sorted = sort(Map, [], maps:size(Map)-1),
    io:format("Sorted : ~p~n", [Sorted]),
    Voila = concat(Sorted),
    io:format("Voila: ~p~n", [Voila]),
    exit(normal);

loop(N, List) ->
    receive
        {sum, Sum, Pos}->
            io:format("FINISHED WITH SUM: ~p~n", [Sum]),
            loop(N - 1, [{Pos,Sum} | List])
    end.


%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(ArgA, ArgB, _Base) ->

    {A, B} = padd(sep(ArgA), sep(ArgB)),
    PairsRev = lists:reverse(split(lists:zip(A, B), 1)),
    Pairs = lists:map(fun lists:reverse/1, PairsRev),
    
    Ppid = self(),
    process_flag(trap_exit,true),
    spawn_link(fun() -> worker(Pairs, Ppid, 0) end),

    io:format("PAIRS ~p~n", [PairsRev]),    
    loop(length(A), []).


%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    tbi.
