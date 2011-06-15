-module(client).

-behaviour(gen_server).

%% ===================================
%% API exports
%% ===================================

-export([]).

%% ===================================
%% gen_server exports
%% ===================================

-export([init/1, terminate/2, code_change/3, 
    handle_call/3, handle_cast/2, handle_info/2]).

%% ====================================
%% external API functions
%% ====================================


% start_link(ArgList) -> 
%     gen_server:start_link({local, ?MODULE}, ?MODULE, ArgList, []).



%% ====================================
%% gen_server callbacks
%% ====================================

init(_Arg) -> 
    State = undefined,
    {ok, State}. 

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%% ------------------------------------
%% gen_server handlers
%% ------------------------------------

handle_call(_Message, _From, State) -> 
    {reply, unknown_call, State}.

handle_cast(_Message, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

%% ------------------------------------
%% internal helper functions
%% ------------------------------------

