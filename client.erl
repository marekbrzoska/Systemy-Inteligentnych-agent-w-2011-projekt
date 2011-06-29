-module(client).

-behaviour(gen_server).

%% ===================================
%% API exports
%% ===================================

-export([start/0, start_link/1]).

%% ===================================
%% gen_server exports
%% ===================================

-export([init/1, terminate/2, code_change/3, 
    handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
        color = undefined,
        server = undefined
    }).

%% ====================================
%% external API functions
%% ====================================

start() -> start_link(game_server).

start_link(ArgList) -> 
    gen_server:start(?MODULE, ArgList, []).



%% ====================================
%% gen_server callbacks
%% ====================================

init(Server) -> 
    case gen_server:call(Server, register) of
        o ->
            error_logger:info_msg("Mam kolor o\n"), 
            State = #state{server=Server, color=o},
            {A1,A2,A3} = now(),
            random:seed(A1, A2, A3),
            {ok, State};
        x ->
            error_logger:info_msg("Mam kolor x\n"),
            State = #state{server=Server, color=x},
            {A1,A2,A3} = now(),
            random:seed(A1, A2, A3),
            {ok, State};
        undefined ->
            error_logger:info_msg("Koncze\n"),
            {stop, koniec}
    end.

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%% ------------------------------------
%% gen_server handlers
%% ------------------------------------

handle_call(_Message, _From, State) -> 
    {reply, unknown_call, State}.

handle_cast({your_turn, Board}, #state{server=Server, color=Color}=State) -> 
    timer:sleep(2000),
    {_Val, Col} = choose_column(Board, Color),
    case gen_server:call(Server, {drop, Color, Col}) of
        {you_win, _NewBoard} ->
            error_logger:info_report({"Polozylem i wygralem", Color, Col}),
            {noreply, State}; 
        {ok, _NewBoard} ->
            {noreply, State};
        Error ->
            error_logger:info_report({"Niedozwolony ruch\n", Error}), 
            {noreply, State}
    end;
handle_cast({you_lose, _}, #state{color=Color}=State) -> 
    error_logger:info_report({"Przegralem", Color}),
    {noreply, State}. 


handle_info(_Info, State) -> 
    {noreply, State}.

%% ------------------------------------
%% internal helper functions
%% ------------------------------------

choose_column(Board, Color) ->
    choose_column(Board, Color, 1, 0, 0).
    
choose_column(Board, Color, 8, BestVal, BestCol) ->
    MyVal = computeVal(Board, Color, 8),
    if
        MyVal > BestVal ->
            {MyVal, 8};
        MyVal == BestVal ->
            case random:uniform(2) of
                1 -> {BestVal, BestCol};
                _ -> {MyVal, 8}
            end;
        true ->
            {BestVal, BestCol}
    end;
choose_column(Board, Color, Col, BestVal, BestCol) ->
    MyVal = computeVal(Board, Color, Col),
    if
        MyVal > BestVal ->
            choose_column(Board, Color, Col+1, MyVal, Col);
        MyVal == BestVal ->
            case random:uniform(2) of
                1 -> choose_column(Board, Color, Col+1, BestVal, BestCol);
                _ -> choose_column(Board, Color, Col+1, MyVal, Col)
            end;
        true ->
            choose_column(Board, Color, Col+1, BestVal, BestCol)
    end.

getOppColor(o) -> x;
getOppColor(x) -> o;
getOppColor(_) -> undefined.

computeVal(Board, Color, Col) ->
    Val = common:drop(Col, Board, Color), 
    case Val of
        {win, _, _} -> 15;
        {ok, _, Max} ->
            OppColor = getOppColor(Color),
            OppVal = common:drop(Col, Board, OppColor),
            case OppVal of
                {win, _, _} -> 14;
                _ ->
                    random:uniform(4) + Max * 4
            end;
        _ -> 0                      
    end.






