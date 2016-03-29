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
worker([H | []], Ppid) -> 
    {Cout,Sum} = calc(H,Ppid,0),
    Ppid ! {sum,Sum},
    exit(Cout);

worker([Head | Pairs],Ppid) ->
    
    {Cout0,Sum0}= calc(Head,Ppid,0),
    {Cout1,Sum1} = calc(Head,Ppid,1),
    
    process_flag(trap_exit,true),
    spawn_link(fun() -> worker(Pairs, Ppid) end),

    receive 
        {'EXIT', _PID, no_carry} ->
            Ppid ! {sum,Sum0},
            exit(Cout0);
        {'EXIT', _PID, carry} ->
            Ppid ! {sum,Sum1},
            exit(Cout1)
    end.



calc([], Cout,Sum) -> 
    io:format("Sum ~p~n", [Cout]),
    
    case Cout =:= 1 of 
        true ->
            {carry,Sum};
        false ->
            {no_carry,Sum}
    end;
    
calc([{A,B} | T], Cin,Sum) -> 
    Tot = A+B+Cin,
    case Tot >= 10 of
        true ->
            calc(T,1,Tot+Sum);
        false ->
            calc(T,0,Tot+Sum)
    end.
    
    


%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(ArgA, ArgB, _Base) ->
    process_flag(trap_exit,true),
    {A, B} = padd(sep(ArgA), sep(ArgB)),
    PairsRev = split(lists:zip(A, B), 1),
    Pairs = lists:map(fun lists:reverse/1, PairsRev),
    Ppid = self(),
    spawn_link(fun() -> worker(Pairs, Ppid) end),
    %%io:format("C ~p~n", [Pairs]).
    
    receive
        {sum,Sum}->
            io:format("Sum ~p~n", [Sum])
    end.


%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    tbi.
