-module(client).

-behaviour(gen_server).

%% ===================================
%% API exports
%% ===================================

-export([start_link/1]).

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


start_link(ArgList) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, ArgList, []).



%% ====================================
%% gen_server callbacks
%% ====================================

init(Server) -> 
    case gen_server:call(Server, register) of
        a ->
            error_logger:info_msg("Mam kolor a\n"), 
            State = #state{server=Server, color=a},
            random:seed(),
            {ok, State};
        b ->
            error_logger:info_msg("Mam kolor b\n"),
            State = #state{server=Server, color=b},
            random:seed(),
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
    {_Val, Col} = choose_column(Board, Color),
    case gen_server:call(Server, {drop, Color, Col}) of
        you_win ->
            error_logger:info_msg("Wygralem\n"),
            {stop, wygrana, State}; 
        ok ->
            error_logger:info_msg("Polozylem\n"), 
            {noreply, State};
        _ ->
            error_logger:info_msg("Niedozwolony ruch\n"), 
            {noreply, State}
    end;
handle_cast(you_lose, State) -> 
    error_logger:info_msg("Przegralem\n"),
    {stop, przegrana, State}. 


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
    MyVal = computeVal(Board, Color, 8),
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

getOppColor(a) -> b;
getOppColor(b) -> a;
getOppColor(_) -> undefined.

computeVal(Board, Color, Col) ->
    Val = common:drop(Col, Board, Color), 
    case Val of
        {win, _Color, _NewBoard} -> 15;
        false -> 0;
        {ok, _NewBoard, Max} ->
            OppColor = getOppColor(Color),
            case common:drop(Col, Board, OppColor) of
                {win, _OppColor, _NewBoard} -> 14;
                _ ->
                    random:uniform(4) + Max * 4
            end;
        _ -> 0                      
    end.






