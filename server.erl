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
        b = undefined          :: pid(),
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
    {ok, State}. 

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%% ------------------------------------
%% gen_server handlers
%% ------------------------------------

handle_call(register, From, #state{a=A, b=B}=State) when A == undefined orelse B == undefined ->
    {A2, B2, Reply} = case A of
        undefined -> {From, B, a};
        _         -> {A, From, b}
    end,
    NewState = State#state{a=A2, b=B2},
    {reply, Reply, NewState};

handle_call(register, _From, State) ->
    {reply, undefined, State};

%handle_call({drop, _, _}, _From, #state{win = Win}=State) when Win /= undefined -> 
handle_call({drop, _, _}, _From, State) -> %when Win /= undefined -> 
    {reply, State, State};

handle_call({drop, Color, N}, From, #state{
                                           win=undefined,
                                           board=Board,
                                           a=A,
                                           b=B,
                                           current=From}=State) 
                                    when   (From == A andalso Color == a) 
                                    orelse (From == B andalso Color == b) ->
    NextPlayer = case From of A -> B; B -> A end,
    case common:drop(Color, N, Board) of
        false ->
            NewState = State,
            Reply = incorrect_move,
            gen_server:cast(NextPlayer, {your_turn, NewState#state.board});
        {ok, NewBoard, _Max} ->
            NewState = State#state{board=NewBoard},
            Reply = ok,
            gen_server:cast(NextPlayer, {your_turn, NewState#state.board});
        {win, Color, NewBoard} ->
            NewState = State#state{win=Color,
                                   board=NewBoard},
            gen_server:cast(NextPlayer, {you_lose, NewState#state.board}),
            Reply = you_win
    end,
    {reply, Reply, NewState#state{current=NextPlayer}};

handle_call(_Message, _From, State) -> 
    {reply, unknown_call, State}.

handle_cast(_Message, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

%% ------------------------------------
%% internal helper functions
%% ------------------------------------
