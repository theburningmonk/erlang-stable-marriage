%% @author theburningmonk

-module(man).
-behaviour(gen_server).

-export([start_link/2, run/1, reject/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name 		= "",
				preferences = [],
				match		= none }).

start_link(Name, Prefs) ->
	gen_server:start_link({global, Name}, ?MODULE, [Name, Prefs], []).

run(Name) ->
	gen_server:cast({global, Name}, run).

reject(Name, Woman) ->
	gen_server:cast({global, Name}, {reject, Woman}).

init([Name, Prefs]) ->
    {ok, #state{name=Name, preferences=Prefs}}.

handle_call(_Request, _From, State) ->
    {reply, unsupported, State}.

handle_cast(_, State=#state{name=Name, preferences=[], match=none}) ->
	ok = stable_marriage:impossible(Name),
    {noreply, State};
handle_cast(run, State=#state{name=Name, preferences=[Woman|T], match=none}) ->
	case woman:propose(Woman, Name) of
		accept -> 
			ok = stable_marriage:matched(Name, Woman),
			{noreply, State#state{preferences=T, match=Woman}};
		reject ->
			run(Name), %% propose to the next woman
			{noreply, State#state{preferences=T}}
	end;
handle_cast({reject, Woman}, State=#state{name=Name, match=Woman}) ->
	ok = stable_marriage:unmatched(Name, Woman),
	run(Name), %% propose to the next woman
    {noreply, State#state{match=none}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.