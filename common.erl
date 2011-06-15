-module(common).
-compile([export_all]).

%-spec drop(integer(), dict(list(atom())), atom()) -> {ok, dict(list(atom()))} | false | {win, atom()}.

example() ->
    dict:from_list([{1, []}, {2, []}, {3, []}, {4, []}, {5, []}, {6, []}, {7, []}, {8, []}]).

drop(Column, State, Color) ->
    List = dict:fetch(Column, State),
    Len = length(List) + 1,
    if 
        Len >= 9 -> false;
        true ->
            LU = left_up(State, Column, Color, Len, 0),
            L  = left(State, Column, Color, Len, 0),
            LD = left_down(State, Column, Color, Len, 0),
            RU = right_up(State, Column, Color, Len, 0),
            R  = right(State, Column, Color, Len, 0),
            RD = right_down(State, Column, Color, Len, 0),
            D  = down(State, Column, Color, Len, 0),
            
            NewState = dict:store(Column, [Color | List], State),
            Max = lists:max([LU + RD, L + R, LD + RU, D]),
            if 
                Max >= 3 -> 
                    {win, Color, NewState};
                true ->      
                    {ok, NewState}
            end
    end.

left_up(_State, 1, _Color, _Row, N)    -> N;
left_up(_State, _Column, _Color, 8, N) -> N;
left_up(State, Column, Color, Row, N) ->
    case dict:find(Column-1, State) of
        error -> N;
        {ok, List} when length(List) < (Row + 1) -> N;
        {ok, List} -> 
            case lists:nth(length(List)-Row, List) of
                Color -> left_up(State, Column-1, Color, Row+1, N+1);
                _     -> N
            end
    end.
    
left(_State, 1, _Color, _Row, N) -> N;
left(State, Column, Color, Row, N) ->
    case dict:find(Column-1, State) of
        error -> N;
        {ok, List} when length(List) < Row -> N;
        {ok, List} -> 
            case lists:nth(length(List)-Row+1, List) of
                Color -> left(State, Column-1, Color, Row, N+1);
                _     -> N
            end
    end.
    
left_down(_State, 1, _Color, _Row, N)    -> N;
left_down(_State, _Column, _Color, 1, N) -> N;
left_down(State, Column, Color, Row, N) ->
    case dict:find(Column-1, State) of
        error -> N;
        {ok, List} when length(List) < (Row - 1) -> N;
        {ok, List} -> 
            case lists:nth(length(List)-Row+2, List) of
                Color -> left_down(State, Column-1, Color, Row-1, N+1);
                _     -> N
            end
    end.   
    
right_up(_State, 8, _Color, _Row, N)    -> N;
right_up(_State, _Column, _Color, 8, N) -> N;
right_up(State, Column, Color, Row, N) ->
    case dict:find(Column+1, State) of
        error -> N;
        {ok, List} when length(List) < (Row + 1) -> N;
        {ok, List} -> 
            case lists:nth(length(List)-Row, List) of
                Color -> right_up(State, Column+1, Color, Row+1, N+1);
                _     -> N
            end
    end.
    
right(_State, 8, _Color, _Row, N) -> N;
right(State, Column, Color, Row, N) ->
    case dict:find(Column+1, State) of
        error -> N;
        {ok, List} when length(List) < Row -> N;
        {ok, List} -> 
            case lists:nth(length(List)-Row+1, List) of
                Color -> right(State, Column+1, Color, Row, N+1);
                _     -> N
            end
    end.
    
right_down(_State, 8, _Color, _Row, N)    -> N;
right_down(_State, _Column, _Color, 1, N) -> N;
right_down(State, Column, Color, Row, N) ->
    case dict:find(Column+1, State) of
        error -> N;
        {ok, List} when length(List) < (Row - 1) -> N;
        {ok, List} -> 
            case lists:nth(length(List)-Row+2, List) of
                Color -> right_down(State, Column+1, Color, Row-1, N+1);
                _     -> N
            end
    end.  
    
down(_State, _Column, _Color, 1, N) -> N;
down(State, Column, Color, Row, N) ->
    case dict:find(Column, State) of
        error -> N;
        {ok, List} -> 
            case lists:nth(length(List)-Row+2, List) of
                Color -> down(State, Column, Color, Row-1, N+1);
                _     -> N
            end
    end.  
        
    
