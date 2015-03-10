%% @author theburningmonk

-module(stable_marriage).
-behaviour(gen_server).

-export([start/0, matched/2, unmatched/2, impossible/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(IDLE_TIMEOUT, 100). % if no activities after 100ms, then the couples have stablized

-record(state, {matched   = maps:new() }).

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

matched(Man, Woman) ->
	gen_server:cast(?MODULE, {matched, Man, Woman}).

unmatched(Man, Woman) ->
	gen_server:cast(?MODULE, {unmatched, Man, Woman}).

impossible(Man) ->
	gen_server:cast(?MODULE, {impossible, Man}).

init([]) ->
	man:start_link("abe",  ["abi","eve","cath","ivy","jan","dee","fay","bea","hope","gay"]),
	man:start_link("bob",  ["cath","hope","abi","dee","eve","fay","bea","jan","ivy","gay"]),
	man:start_link("col",  ["hope","eve","abi","dee","bea","fay","ivy","gay","cath","jan"]),
	man:start_link("dan",  ["ivy","fay","dee","gay","hope","eve","jan","bea","cath","abi"]),
	man:start_link("ed",   ["jan","dee","bea","cath","fay","eve","abi","ivy","hope","gay"]),
	man:start_link("fred", ["bea","abi","dee","gay","eve","ivy","cath","jan","hope","fay"]),
	man:start_link("gav",  ["gay","eve","ivy","bea","cath","abi","dee","hope","jan","fay"]),
	man:start_link("hal",  ["abi","eve","hope","fay","ivy","cath","jan","bea","gay","dee"]),
	man:start_link("ian",  ["hope","cath","dee","gay","bea","abi","fay","ivy","jan","eve"]),
	man:start_link("jon",  ["abi","fay","jan","gay","eve","bea","dee","cath","ivy","hope"]),

	woman:start_link("abi",  ["bob","fred","jon","gav","ian","abe","dan","ed","col","hal"]),
	woman:start_link("bea",  ["bob","abe","col","fred","gav","dan","ian","ed","jon","hal"]),
	woman:start_link("cath", ["fred","bob","ed","gav","hal","col","ian","abe","dan","jon"]),
	woman:start_link("dee",  ["fred","jon","col","abe","ian","hal","gav","dan","bob","ed"]),
	woman:start_link("eve",  ["jon","hal","fred","dan","abe","gav","col","ed","ian","bob"]),
	woman:start_link("fay",  ["bob","abe","ed","ian","jon","dan","fred","gav","col","hal"]),
	woman:start_link("gay",  ["jon","gav","hal","fred","bob","abe","col","ed","dan","ian"]),
	woman:start_link("hope", ["gav","jon","bob","abe","ian","dan","hal","ed","col","fred"]),
	woman:start_link("ivy",  ["ian","col","hal","gav","fred","bob","abe","ed","jon","dan"]),
	woman:start_link("jan",  ["ed","hal","gav","abe","bob","jon","col","ian","fred","dan"]),
	
	man:run("abe"),
	man:run("bob"),
	man:run("col"),
	man:run("dan"),
	man:run("ed"),
	man:run("fred"),
	man:run("gav"),
	man:run("hal"),
	man:run("ian"),
	man:run("jon"),
	
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, unsupported, State}.

handle_cast({matched, Man, Woman}, State=#state{matched=Matched}) ->
	io:format("~s is engaged to ~s~n", [Man, Woman]),
	
	NewMatched 	 = maps:put(Man, Woman, Matched),	
    {noreply, #state{matched=NewMatched}, ?IDLE_TIMEOUT};
handle_cast({unmatched, Man, Woman}, State=#state{matched=Matched}) ->
	io:format("~s is separated from ~s~n", [Man, Woman]),
	
	NewMatched 	 = maps:remove(Man, Matched),	
    {noreply, #state{matched=NewMatched}, ?IDLE_TIMEOUT};
handle_cast({impossible, Man}, State=#state{matched=Matched}) ->
	io:format("~s is bound to die alone...~n", [Man]),
	
	NewMatched 	 = maps:remove(Man, Matched),
    {noreply, #state{matched=NewMatched}, ?IDLE_TIMEOUT}.

handle_info(timeout, State=#state{matched=Matched}) ->
	io:format("~nMarriages have stablized...~n", []),
	[io:format("~s is married to ~s~n", [Man, Woman])
	 || {Man, Woman} <- maps:to_list(Matched) ],

    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.