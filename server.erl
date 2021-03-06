-module(server).

-behaviour(gen_server).

-define(MAX, 8).

%% ===================================
%% API exports
%% ===================================

-export([start/0, start_link/1, restart/1, restart/0]).

%% ===================================
%% gen_server exports
%% ===================================

-export([init/1, terminate/2, code_change/3, 
    handle_call/3, handle_cast/2, handle_info/2]).

%% ====================================
%% external API functions
%% ====================================

start() -> start_link(ok).

start_link(ArgList) -> 
	error_logger:info_report({serwer, "Proba wystartowania serwera...\n"}),
	timer:sleep(500),
	try
    	gen_server:start_link({local, game_server}, ?MODULE, ArgList, [])
    catch
    	_:_ -> 
    		error_logger:info_report({serwer, "Serwer jest juz wystartowany\n"}),
    		koniec
    end.

restart() ->
    gen_server:cast(game_server, restart).

restart(brutal) ->
    gen_server:cast(game_server, brutal_restart).

%% ====================================
%% internal state
%% ====================================

-record(state, {
        win = undefined        :: atom(),
        board                  :: dict(),
        last_color = undefined :: atom(),
        nr_of_players = 0      :: integer(),
        o = undefined          :: pid(),
        x = undefined          :: pid(),
        current = undefined    :: pid()
    }).


%% ====================================
%% gen_server callbacks
%% ====================================

init(_Arg) -> 
    Keys = lists:seq(1,?MAX),
    KV = lists:zip(Keys, lists:map(fun(_) -> [] end, Keys)),
    Board = dict:from_list(KV),
    State = #state{board = Board},
    error_logger:info_report({serwer, "Serwer wystartowal\n"}),
    {ok, State}. 

terminate(_Reason, _State) -> 
    error_logger:info_report({serwer, "Serwer zakonczyl dzialanie\n"}),
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%% ------------------------------------
%% gen_server handlers
%% ------------------------------------

handle_call(register, {From, _}, #state{o=A, x=B, board=Board}=State) when A == undefined orelse B == undefined ->
    {A2, B2, Reply} = case A of
        undefined -> {From, B, o};
        _         -> {A, From, x}
    end,
    if 
        A2 /= undefined andalso B2 /= undefined ->
            Current = A2,
            gen_server:cast(Current, {your_turn, Board});
        true -> 
            Current = undefined
    end,
            
    NewState = State#state{o=A2, x=B2, current=Current},
    {reply, Reply, NewState};

handle_call(register, _From, State) ->
    {reply, undefined, State};


handle_call({drop, _, _}, _From, #state{win = Win}=State) when Win /= undefined -> 
%handle_call({drop, _, _}, _From, State) -> %when Win /= undefined -> 
    {reply, State, State};

handle_call({drop, Color, N}, {From,_}, #state{
                                           win=undefined,
                                           board=Board,
                                           o=A,
                                           x=B,
                                           current=From}=State) 
                                    when   (From == A andalso Color == o) 
                                    orelse (From == B andalso Color == x) ->
    NextPlayer = case From of A -> B; B -> A end,
    case common:drop(N, Board, Color) of
        false ->
            NewState = State,
            Reply = incorrect_move,
            gen_server:cast(NextPlayer, {your_turn, NewState#state.board});
        {ok, NewBoard, _Max} ->
            NewState = State#state{board=NewBoard},
            Reply = {ok, NewBoard},
            common:display(NewBoard),
            gen_server:cast(NextPlayer, {your_turn, NewState#state.board});
        {win, Color, NewBoard} ->
            NewState = State#state{win=Color,
                                   board=NewBoard},
            gen_server:cast(NextPlayer, {you_lose, NewState#state.board}),
            Reply = {you_win, NewBoard},
            common:display(NewBoard)
    end,
    {reply, Reply, NewState#state{current=NextPlayer}};

handle_call(Message, From, State) -> 
    error_logger:info_report(last_call),
    {reply, {State, Message, From}, State}.

handle_cast(restart, #state{win=Winner}) when Winner /= undefined->
    error_logger:info_report([{restarting, now}, {last_winner, Winner}]),
    {ok, State} = init(any),
    {noreply, State};

handle_cast(brutal_restart, _) ->
    error_logger:info_report([{restarting, now}
                            , {breaking_game, no_winner}]),
    {ok, State} = init(any),
    {noreply, State};

handle_cast(_Message, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

%% ------------------------------------
%% internal helper functions
%% ------------------------------------
