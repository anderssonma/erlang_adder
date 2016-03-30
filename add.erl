%% @doc Erlang mini project.
-module(add).
-export([m/0, n/1, start/3, start/4, split/2, padd/2]).

%Work?

m() -> start(123456789123456789, 345678912345648912, 20).
n(N) ->start(123456789123456789, 345678912345648912, 10 ,N ).
    


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

%% Seperate each individual number into an element in a list
sep(0, Acc) -> Acc;
sep(N, Acc) ->
    Next = N rem 10,
    %%io:format("Kvar  N ~p~n", [N]),
    %%io:format("Next ~p~n", [Next]),
    sep(trunc(N div 10), [Next | Acc]).

sep(N) -> sep(N, []).

%% Insert padding in the list if not of equal length
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
    {Cout, Sum} = calc(H, 0, []),
    %%io:format("Worker slut i listan: ~p ~n", [H]),
    %%io:format("Count: ~p Sum: ~p ~n", [Count], [Sum]),
    Ppid ! {sum, Sum , Count},
    %%io:format("Count: ~p ~n", [Count]),
    exit(Cout);

worker([Head | Pairs], Ppid, Count) ->
        
    process_flag(trap_exit,true),
    spawn_link(fun() -> worker(Pairs, Ppid,Count+1) end),

    {Cout0, Sum0} = calc(Head, 0, []),
    {Cout1, Sum1} = calc(Head, 1, []),
    %%io:format("Count: ~p ~n", [Head]),
    receive 
        {'EXIT', _PID, no_carry} ->
            Ppid ! {sum, Sum0 , Count},
            %%io:format("Count: ~p ~n", [Count]),
            exit(Cout0);
        {'EXIT', _PID, carry} ->
            Ppid ! {sum, Sum1 , Count},
            %%io:format("Count: ~p ~n", [Count]),
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
    %%io:format("A: ~p ~n", [A]),
    %%io:format("B: ~p ~n", [B]),
    case Tot >= 10 of
        true ->
            calc(T, 1, [Tot rem 10 | Sum]);
        false ->
            calc(T, 0, [Tot rem 10 | Sum])
    end.


%% Sort the received sums in correct order and return correct total sum
sort(Map,List,0) -> 
    SortedList = lists:concat(maps:get(0, Map) ++ List),
    %%io:format("sorted List : ~p~n", [SortedList]),
    {Result , _} = string:to_integer(SortedList),
    Result;

sort(Map,List,Pos) ->
    sort(Map,maps:get(Pos, Map) ++ List,Pos-1).
    
    
loop(0, List) -> 
    %%io:format("loop Innan Map : ~p~n", [List]),
    Map = maps:from_list(List),
    %%io:format(" Map : ~p~n", [Map]),
    Sorted = sort(Map, [], maps:size(Map)-1),
    io:format("Correct result : ~p~n", [Sorted]),
    exit(normal);

loop(N, List) ->
    receive
        {sum, Sum, Pos}->
            %%io:format("Received in loop: ~p~n", [Pos]),
            loop(N - 1, [{Pos,Sum} | List])
    end.


%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(ArgA, ArgB, _Base) ->
    
    {A, B} = padd(sep(ArgA),sep(ArgB)),
    
    PairsRev = lists:reverse(split(lists:zip(A, B), 1)),
    %%io:format("PairsRev: ~p~n", [PairsRev]),
    Pairs = lists:map(fun lists:reverse/1, PairsRev),
    %%io:format("PAirs: ~p~n", [Pairs]),
    Ppid = self(),
    process_flag(trap_exit,true),
    spawn_link(fun() -> worker(Pairs, Ppid, 0) end),

    %%io:format("PAIRS ~p~n", [PairsRev]),    
    loop(length(A) , []).


%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(ArgA,ArgB,_Base, Options) ->
    
    {A, B} = padd(sep(ArgA),sep(ArgB)),
    %%io:format("A: ~p~n", [A]),
    %%io:format("B: ~p~n", [B]),

    C = lists:zip(A, B),
    %%io:format("C: ~p~n", [C]),

    PairsRev = lists:reverse(utils:split(C , Options)),
    %%io:format("PairsRev: ~p~n", [PairsRev]),
    Pairs = lists:reverse(lists:map(fun lists:reverse/1, PairsRev)),
    %%io:format("PAirs: ~p~n", [Pairs]),
    Ppid = self(),
    process_flag(trap_exit,true),
    spawn_link(fun() -> worker(Pairs, Ppid, 0) end),

    %%io:format("PAIRS ~p~n", [PairsRev]),    
    loop(Options , []).
