%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4, split/2, padd/2]).

%Work?

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
    io:format("N ~p~n", [N]),
    io:format("Next ~p~n", [Next]),
    sep(trunc(N/10), [Next|Acc]).

sep(N) -> sep(N, []).


padd(A, B) when length(A) =:= length(B) ->
    {A, B};
padd(A, B) when length(A) >= length(B) ->
    padd(A, [0|B]);
padd(A, B) ->
    padd([0|A], B).

%% divisible(M, N) when M / N =:= 0 ->
%%     M / N;
%% divisible(M, N) ->
%%     N + 1.





%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(ArgA, ArgB, _Base) ->
    
    {A, B} = padd(sep(ArgA), sep(ArgB)),
    PairsRev = split(lists:zip(A, B), 1),
    Pairs = lists:map(fun lists:reverse/1, PairsRev),
    
    io:format("C ~p~n", [Pairs]).
    

%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    tbi.
