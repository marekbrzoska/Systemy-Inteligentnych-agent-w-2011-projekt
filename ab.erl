-module(ab).

-behaviour(gen_server).

%% ===================================
%% API exports
%% ===================================

-export([start/0]).

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

start() -> start(game_server).

start(ArgList) ->
	error_logger:info_msg("Proba polaczenia z serwerem gry...\n"),
    timer:sleep(2000), 
    gen_server:start(?MODULE, ArgList, []).


%% ====================================
%% gen_server callbacks
%% ====================================

init(Server) -> 
    try gen_server:call(Server, register) of
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
        _ ->
            error_logger:info_msg("Nieznana odpowiedz. Sprobuj ponownie\n"),
            {stop, koniec}
    catch
    	_:_ ->
    		error_logger:info_msg("Serwer nie odpowiada. Sprobuj ponownie.\n"),
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
    try gen_server:call(Server, {drop, Color, Col}) of
        {you_win, _NewBoard} ->
            error_logger:info_report({Color, "Wygralem"}),
            {noreply, wygrana}; 
        {ok, _NewBoard} ->
            {noreply, State};
        _Error ->
            error_logger:info_report({Color, "Wykonalem niedozwolony ruch"}), 
            {noreply, State}
    catch
    	_:_ -> 
    		error_logger:info_report({Color, "Serwer nie odpowiedzial. Koniec gry"}), 
            {noreply, koniec}
    end;
handle_cast({you_lose, _}, #state{color=Color}) -> 
    error_logger:info_report({Color, "Przegralem"}),
    {noreply, przegrana}. 

handle_info(_Info, State) -> 
    {noreply, State}.

%% ------------------------------------
%% internal helper functions
%% ------------------------------------

choose_column(Board, Color) -> %wybor najlepszej kolumny (liczymy wartosci dla kolejnych mozliwych ruchow, wybieramy najlepszy ruch (kolumne))
    %choose_column(Board, Color, 1, 0, 0).
    mainMaxAB(Board, 1, 6, -17, 17, Color, 1).
    
choose_column(Board, Color, 8, BestVal, BestCol) ->
    %MyVal = computeVal(Board, Color, 8),
    MyVal = alphabeta(Board, 8, 4, -17, 17, getOppColor(Color), Color),
    %MyVal = minimax(Board, 8, 4, Color),
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
    %MyVal = computeVal(Board, Color, Col),
    MyVal = alphabeta(Board, Col, 4, -17, 17, getOppColor(Color), Color),
    %MyVal = minimax(Board, Col, 4, Color),
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

computeVal(Board, Color, Col) -> %obliczenie wartosci dla danego ruchu (wrzucenie do danej kolumny)
    Val = common:drop(Col, Board, Color), 
    case Val of
    	false -> -17;
        {win, _, _} -> 17; %moja wygrana po ruchu
        {ok, NewBoard, Max} ->
            OppColor = getOppColor(Color),
            OppVal = common:drop(Col, Board, OppColor),
            case OppVal of
                {win, _, _} -> %przeciwnik wygra wrzucajac do tej kolumny ruchu (chcemy tu wrzucic)
                	case checkNextTurn(NewBoard, OppColor) of
                		win    -> 15; %przeciwnik ma ruch wygrywajacy po naszym ruchu
                		no_win -> 16  %nie ma
                	end;
                _ -> %przeciwnik nie wygra
                	case checkNextTurn(NewBoard, OppColor) of
                		win -> -16;
                		no_win ->
				        	case checkWinningMove(Board, Color, Col) of
				        		yes -> 14; %mamy ruch wygrywajacy (utworzymy trzy krazki w linii, niezablokowane)
				        		no ->
				        			case checkWinningMove(Board, OppColor, Col) of
				        				yes -> 13; %przeciwnik bedzie mial taki ruch wygrywajacy po wrzuceniu tutaj
				        				no -> random:uniform(2) + Max * 2 %zadne z powyzszych, chcemy utworzyc jak najdluzszy lancuch                	                	
									end
							end
					end
            end;
        _ -> -17                      
    end.

checkNextTurn(Board, Color) -> checkNextTurn(Board, Color, 1). %sprawdzenie czy po naszym ruchu przeciwnik ma ruch wygrywajacy

checkNextTurn(_Board, _Color, 9) -> no_win;
checkNextTurn(Board, Color, Col) -> 
	Val = common:drop(Col, Board, Color), 
	case Val of
		{win, _, _} -> win;
		_ -> checkNextTurn(Board, Color, Col + 1)
	end.


checkWinningMove(Board, Color, Column) -> %sprawdzenie czy mozemy doprowadzic do sytuacji _xxx_ (pewne wygrana), jesli nie, to czy przeciwnik moze
	Len = length(dict:fetch(Column, Board)),
	Left = common:left(Board, Column, Color, Len + 1, 0),
	Right = common:right(Board, Column, Color, Len + 1, 0),
	
	if
		Left + Right >= 2 ->
			if 
				((Column - Left) > 1) and ((Column + Right) < 8) ->
					LenLeft = length(dict:fetch(Column - Left - 1, Board)),
					LenRight = length(dict:fetch(Column + Right + 1, Board)),
					if 
						(LenLeft == LenRight) and (LenRight == Len) -> 
							{ok, NewBoard, _Max} = common:drop(Column, Board, Color),
							OppColor = getOppColor(Color),
							case checkNextTurn(NewBoard, OppColor) of
								win -> no;
								no_win -> yes
							end;						
						true -> no
					end;
				true -> no
			end;
		true -> no
	end.

alphabeta(Board, Column, 0, _A, _B, Color, _MyColor) -> 
	computeVal(Board, Color, Column);

alphabeta(Board, Column, Depth, A, B, Color, MyColor) when Color == MyColor ->
	case common:drop(Column, Board, Color) of
		false -> -17;
		{win, _, _} -> 17;
		{ok, NewBoard, _Max} -> maxAB(NewBoard, 1, Depth - 1, A, B, getOppColor(Color), MyColor)
	end;
		
alphabeta(Board, Column, Depth, A, B, Color, MyColor) ->
	case common:drop(Column, Board, Color) of
		false -> -17;
		{win, _, _} -> 17;
		{ok, NewBoard, _Max} -> minAB(NewBoard, 1, Depth - 1, A, B, getOppColor(Color), MyColor)
	end.


maxAB(_Board, 9, _Depth, A, _B, _Color, _MyColor) -> A;

maxAB(Board, Column, Depth, A, B, Color, MyColor) ->
	AB = alphabeta(Board, Column, Depth, A, B, Color, MyColor),
	NewA = erlang:max(AB, A),
	if 
		B =< NewA -> NewA;
		true -> maxAB(Board, Column + 1, Depth, NewA, B, Color, MyColor)
	end. 


minAB(_Board, 9, _Depth, _A, B, _Color, _MyColor) -> B;

minAB(Board, Column, Depth, A, B, Color, MyColor) ->
	AB = alphabeta(Board, Column, Depth, A, B, Color, MyColor),
	NewB = erlang:min(AB, B),
	if 
		NewB =< A -> NewB;
		true -> minAB(Board, Column + 1, Depth, A, NewB, Color, MyColor)
	end. 


mainMaxAB(_Board, 9, _Depth, A, _B, _MyColor, Best) -> {A, Best};

mainMaxAB(Board, Column, Depth, A, B, MyColor, Best) ->
	AB = alphabeta(Board, Column, Depth, A, B, getOppColor(MyColor), MyColor),
	{NewA, NewBest} = 
	if
		AB > A -> {AB, Column};
		AB == A -> 
			case random:uniform(2) of
				1 -> {AB, Column};
				_ -> {A, Best}
			end;
		true -> {A, Best}
	end,
	if
		B =< NewA -> {NewA, NewBest};
		true -> mainMaxAB(Board, Column + 1, Depth, NewA, B, MyColor, NewBest)
	end.







