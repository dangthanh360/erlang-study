%% @doc @todo Calculate PI using <a href="https://www.geeksforgeeks.org/estimating-value-pi-using-monte-carlo/">Monte Carlo</a> method. Concurrency implements

-module(pi_con).

-export([run/2]).


%% ====================================================================
%% API functions
%% ====================================================================
run(Times, Workers) ->
    Begining = get_timestamp(),
    
    Hash = Times div Workers,
    Parts = hash_data(Hash, Times, 0),
    WorkerList = create_workers(Parts),
    send_data(WorkerList, calc),
    send_data(WorkerList, collect),
    
    {PointsInCircle, PointsInSquare} = sum_points(0, 0, length(WorkerList)),
    Pi = 4 * (PointsInCircle / PointsInSquare),
    io:fwrite("Pi: ~p~n", [Pi]),

    ElapsedTime = (get_timestamp() - Begining) / 1000,
    io:format("Elapsed time: ~p seconds ~n", [ElapsedTime]).


%% ====================================================================
%% Master related functions
%% ====================================================================
hash_data(Hash, Times, Start) when Start == 0; Start + Hash < Times ->
    Parts = hash_data(Hash, Times, Start + Hash),
    [{Start, Start + Hash - 1}] ++ Parts;
hash_data(Hash, Times, Start) when Start + Hash == Times; Start + Hash > Times ->
    [{Start, Times}].

create_workers(HashData) ->
    Fun = fun(Element, WorkerList) ->
        {From, To} = Element,
        Pid = spawn(fun() -> calculate(0, 0) end),
        WorkerList ++ [{Pid, From, To}]
    end,
    lists:foldl(Fun, [], HashData).

send_data(WorkerList, Work) ->
    Fun = fun(Element) ->
        {Pid, From, To} = Element,
        Pid ! {self(), From, To, Work}
    end,
    lists:foreach(Fun, WorkerList).

sum_points(NumPointsInCircle, NumPointsInSquare, 0) ->
    {NumPointsInCircle, NumPointsInSquare};
sum_points(NumPointsInCircle, NumPointsInSquare, PendingWorkers) ->
    receive
        {_WorkerPid, Circle, Square} ->
            NewPointsInCircle = NumPointsInCircle + Circle,
            NewPointsInSquare = NumPointsInSquare + Square,
            
            sum_points(NewPointsInCircle, NewPointsInSquare, PendingWorkers - 1)
    end.

get_timestamp() ->
   {Mega, Sec, Micro} = os:timestamp(),
   (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).


%% ====================================================================
%% Worker related functions
%% ====================================================================
calculate(NumPointsInCircle, NumPointsInSquare) ->
    receive
        {MasterPid, _Start, _End, collect} ->
            MasterPid ! {self(), NumPointsInCircle, NumPointsInSquare};
        {_MasterPid, Start, End, calc} ->
            {PointsInCircle, PointsInSquare} = generate_points(Start, End),
            calculate(NumPointsInCircle + PointsInCircle, NumPointsInSquare + PointsInSquare)
    end.

generate_points(From, To) ->
    rand:seed (exs1024s),
    Points = [{gen_random_number(), gen_random_number()} || _ <- lists:seq(From, To)],
    calc_points(Points, 0, 0).

calc_points([H|T], NumPointsInCircle, NumPointsInSquare) ->
    {X, Y} = H,
    NumPointsInSquareNew = NumPointsInSquare + 1,
    InCircle = is_in_circle(X, Y),
    if
        InCircle    -> NumPointsInCircleNew = NumPointsInCircle + 1;
        true        -> NumPointsInCircleNew = NumPointsInCircle
    end,
    
    calc_points(T, NumPointsInCircleNew, NumPointsInSquareNew);
calc_points([], NumPointsInCircle, NumPointsInSquare) -> 
    {NumPointsInCircle, NumPointsInSquare}.

gen_random_number() ->
    rand:uniform(100000) / 100000.

sqr(X) ->
    X * X.

distance(X, Y) ->
    sqr(X) + sqr(Y).

is_in_circle(X, Y) -> 
    distance(X, Y) < 1.
