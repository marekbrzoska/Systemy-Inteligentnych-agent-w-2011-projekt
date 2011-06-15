-module(server).

-behaviour(gen_server).

-define(MAX, 8).

%% ===================================
%% API exports
%% ===================================

-export([start_link/1]).

%% ===================================
%% gen_server exports
%% ===================================

-export([init/1, terminate/2, code_change/3, 
    handle_call/3, handle_cast/2, handle_info/2]).

%% ====================================
%% external API functions
%% ====================================


start_link(ArgList) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, ArgList, []).


%% ====================================
%% internal state
%% ====================================

-record(state, {
        win = undefined        :: atom(),
        board                  :: dict(),
        last_color = undefined :: atom(),
        nr_of_players = 0      :: integer(),
        a = undefined          :: pid(),
        b = undefined          :: pid()
    }).


%% ====================================
%% gen_server callbacks
%% ====================================

init(_Arg) -> 
    Keys = lists:seq(1,?MAX),
    KV = lists:zip(Keys, lists:map(fun(_) -> [] end, Keys)),
    Board = dict:from_list(KV),
    State = #state{board = Board},
    {ok, State}. 

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%% ------------------------------------
%% gen_server handlers
%% ------------------------------------

handle_call(register, From, #state{a=A, b=B}) when A == undefined orelse B == undefined ->
    {A2, B2, Reply} = case A of
        undefined -> {From, B, a};
        _         -> {A, From, b}
    end,
    NewState = State#state{a=A2, b=B2},
    {reply, Reply, NewState}.

handle_call(_Message, _From, #state{win = Win}=State) when Win /= undefined -> 
    {reply, State, State};
handle_call({drop, Color, N}, _From, #state{last_color=LastColor, 
                                            win=undefined,
                                            board=Board}=State) 
                                    when LastColor /= Color ->
    case drop(Color, N, Board) of
        false ->
            NewState = State#state{last_color=Color},
            Reply = incorrect_move;
        {ok, NewBoard} ->
            NewState = State#state{last_color=Color,
                                   board=NewBoard},
            Reply = ok;
        {win, Color, NewBoard} ->
            NewState = State#state{last_color=Color,
                                   win=Color,
                                   board=NewBoard},
            Reply = you_win
    end,
    {reply, Reply, NewState};

handle_call(_Message, _From, State) -> 
    {reply, unknown_call, State}.

handle_cast(_Message, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

%% ------------------------------------
%% internal helper functions
%% ------------------------------------
