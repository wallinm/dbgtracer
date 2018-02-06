-module(dbgtracer_stats).

-export([trace_event/1]).
-export([print_call_stats/0]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-behaviour(gen_server).

-record(state, {call_stat_dict = undefined}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init(_Args) ->
  State = #state{call_stat_dict = dict:new()},
  {ok, State}.

trace_event(Trace) ->
  gen_server:cast(?SERVER, {trace_event, Trace}).

print_call_stats() ->
  gen_server:cast(?SERVER, print_call_stats).

handle_call(Request, From, State) ->
  io:format("~p:handle_call: unexpected request ~p from ~p~n", [?SERVER, Request, From]),
  {reply, ok, State}.

handle_cast({trace_event, Trace}, State) ->
  NewDict =
    case Trace of
      {trace_ts, _Pid, call, {M, F, _A}, _T} ->
  	    Key = atom_to_list(M) ++ ":" ++ atom_to_list(F),
  	    dict:update_counter(Key, 1, State#state.call_stat_dict);
      {trace_ts, _Pid, return_from, {_M, _F, _A}, _Reply, _T} ->
        % ignore return trace
        State#state.call_stat_dict;
      _ ->
        dict:update_counter(undefined, 1, State#state.call_stat_dict)
    end,
  NewState = State#state{call_stat_dict = NewDict},
  {noreply, NewState};
handle_cast(print_call_stats, State) ->
  UnsortedList = dict:to_list(State#state.call_stat_dict),
  % sort on the increment: [{Key, Increment}, ...]
  SortedList = lists:keysort(2, UnsortedList),
  io:format("~p~n", [SortedList]),
  NewState = State,
  {noreply, NewState};
handle_cast(Request, State) ->
  io:format("~p:handle_cast: unexpected request ~p~n", [?SERVER, Request]),
  NewState = State,
  {noreply, NewState}.  

 handle_info(_Info, State) ->
  %% TODO
  NewState = State,
  {noreply, NewState}. 

code_change(_OldVsn, State, _Extra) ->
  %% TODO
  NewState = State,
  {ok, NewState}.

terminate(_Reason, _State) ->
  ok.