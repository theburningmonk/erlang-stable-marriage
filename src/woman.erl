%% @author theburningmonk

-module(woman).
-behaviour(gen_server).

-export([start_link/2, propose/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name 		= "",
				preferences = [],
				match		= none }).

start_link(Name, Prefs) ->
	gen_server:start_link({global, Name}, ?MODULE, [Name, Prefs], []).

propose(Name, Man) ->
	gen_server:call({global, Name}, {proposal, Man}).

init([Name, Prefs]) ->
    {ok, #state{name=Name, preferences=Prefs}}.

handle_call({proposal, Man}, _From, State=#state{match=none}) ->
    {reply, accept, State#state{match=Man}};
handle_call({proposal, Man}, _From, State=#state{name=Name, preferences=Prefs, match=Match}) ->
	case is_better(Prefs, Match, Man) of
		true -> %% trade up
			man:reject(Match, Name),
			{reply, accept, State#state{match=Man}};
		false ->
			{reply, reject, State}
	end.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

is_better([H|T], Current, New) ->
	case H of
		New -> true;
		Current -> false;
		_ -> is_better(T, Current, New)
	end.