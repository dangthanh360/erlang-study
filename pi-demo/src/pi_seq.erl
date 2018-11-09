%% @doc @todo Calculate PI using <a href="https://www.geeksforgeeks.org/estimating-value-pi-using-monte-carlo/">Monte Carlo</a> method. Sequential implements


-module(pi_seq).

-export([run/2]).


%% ====================================================================
%% API functions
%% ====================================================================
run(Times, Workers) ->
    Begining = get_timestamp(),
    
    Hash = Times div Workers,
    Points = hash_data(Hash, Times, 0),
    {PointsInCircle, PointsInSquare} = sum_points(Points),
    Pi = 4 * (PointsInCircle / PointsInSquare),
    io:fwrite("Pi: ~p~n",[Pi]),
    
    ElapsedTime = (get_timestamp() - Begining) / 1000,
    io:format("Elapsed time: ~p seconds ~n", [ElapsedTime]).


%% ====================================================================
%% Internal functions
%% ====================================================================
sum_points(L) -> 
   sum_points(L, {0, 0}).
sum_points([H|T], {Circle, Square}) ->
    {C, S} = H,
    sum_points(T, {C + Circle, S + Square});
sum_points([], {Circle, Square}) ->
   {Circle, Square}.

hash_data(Hash, Times, Start) when Start == 0; Start + Hash < Times ->
    Points = generate_points(Start, Start + Hash - 1),
    H = hash_data(Hash, Times, Start + Hash),
    Points ++ H;

hash_data(Hash, Times, Start) when Start + Hash == Times; Start + Hash > Times ->
    generate_points(Start, Times).

generate_points(From, To) ->
    rand:seed (exs1024s),
    Points = [{gen_random_number(), gen_random_number()} || _ <- lists:seq(From, To)],
    [calc_points(Points, 0, 0)].

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

get_timestamp() ->
   {Mega, Sec, Micro} = os:timestamp(),
   (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).
