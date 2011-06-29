-module(manual).

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
            error_logger:info_msg("Masz kolor o\n"), 
            State = #state{server=Server, color=o},
            {ok, State};
        x ->
            error_logger:info_msg("Masz kolor x\n"),
            State = #state{server=Server, color=x},
            {ok, State};
        undefined ->
            error_logger:info_msg("Koniec\n"),
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

handle_cast({your_turn, _Board}, #state{server=Server, color=Color}=State) -> 
    Col = hd(io:get_line("podaj numer kolumny (1-8): > ")) - $0,
    case gen_server:call(Server, {drop, Color, Col}) of
        {you_win, _NewBoard} ->
            error_logger:info_msg("Wygrales!"),
            {noreply, State}; 
        {ok, _NewBoard} ->
            {noreply, State};
        Error ->
            error_logger:info_report({"Niedozwolony ruch\n", Error}), 
            {noreply, State}
    end;
handle_cast({you_lose, _Board}, State) -> 
    error_logger:info_msg("Przegrales..."),
    {noreply, State}. 


handle_info(_Info, State) -> 
    {noreply, State}.
