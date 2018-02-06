%%
%% example usage:
%% server side:
%%   dbg_trace_server:start().
%%   dbg_trace_server:dbg_tp_all().
%%
%% client side:
%%    dbg_trace_client:start().
%%    
%% logging:
%%   tail -f dbg_trace.log | tee <output-log-file>
%%
%% erl shell:
%%   code:add_path("/Users/wallinm/Documents/workspace/MyErl/src").
%%   l(dbg_trace_server).
%%   dbg_trace_server:start().
%%
-module(dbgtracer_engine).

%-define(DBG_TRACE_PORT, 4711).
%-define(DBG_TRACE_PORT_QUE_SIZE, 1000).
%-define(DBG_TRACE_NODE, 'cepheid@localhost').

-define(DBG_TRACE_CLIENT_NAME, dbg_trace_client).
%-define(DBG_TRACE_FILE, "dbg_trace.log").
-define(SERVER, ?MODULE).

-record(state, {all_loaded_modules,
                all_loaded_modules_sorted,
                iodevice,
                tpl,
                tracenode}).

-export([start_link/0]).
-export([all_loaded/0]).
-export([all_loaded_sorted/0]).

-export([call/1, cast/1]).

% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([stop/0]).
-export([handle_trace/2]).
-export([get_all_loaded_modules/0]).

-behaviour(gen_server).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init(_Args) ->
  %dbgtracer_api:module_info(),
  Test = application:get_env(log_dir),
  io:format("TEST: ~p~n", [Test]),
  {ok, LogDir} = application:get_env(log_dir),
  {ok, LogFileName} = application:get_env(log_file_name),
  {ok, TracePort} = application:get_env(trace_port),
  {ok, TracePortQueueSize} = application:get_env(trace_port_queue_size),
  {ok, TraceNode} = application:get_env(trace_node),
  {ok, TraceModules} = application:get_env(trace_modules),
  {ok, TraceFunction} = application:get_env(trace_function),
  LogFilePath = LogDir ++ "/" ++ LogFileName,
  %LogFilePath = ?DBG_TRACE_FILE,
  Res = start_dbg(TraceNode, TracePort, TracePortQueueSize),
  case Res of
    {ok, Node} ->
      io:format("~p Start tracer on node: ~p~n", [?MODULE, Node]);
    {error, Reason} ->
      io:format("~p Failed to start tracer: ~p~n", [?MODULE, Reason])
  end,

  AllLoaded =
    case rpc:call(TraceNode, code, all_loaded, []) of
      {badrpc, AllLoadedReason} ->
        io:format("Failed rpc:call(~p, ~p, ~p): ~p~n", [TraceNode, code, all_loaded, AllLoadedReason]),
        [];
      AllLoadedRes ->
        AllLoadedRes
    end,

  InitialData = 0, % The initial value of Arg2 in handler function
  TraceClientPid = dbg:trace_client(ip, TracePort, {fun ?MODULE:handle_trace/2, InitialData}),
  io:format("init: TraceClientPid: ~p~n", [TraceClientPid]),

  io:format("init: LogFilePath: ~p~n", [LogFilePath]),
  {ok, IoDevice} = file:open(LogFilePath, [write]),

  cast({enable_dbg, TraceFunction, TraceModules}),

  State = #state{all_loaded_modules = AllLoaded,
                 all_loaded_modules_sorted = sort_loaded_modules(AllLoaded),
                 iodevice = IoDevice,
                 tpl = [],
                 tracenode = TraceNode},
  {ok, State}.

start_dbg(TraceNode, TracePort, TracePortQueueSize) ->
  Type = port,
%  Data = dbg:trace_port(ip, {TracePort, TracePortQueueSize}),
  rpc:call(TraceNode, dbg, tracer, [Type, dbg:trace_port(ip, {TracePort, TracePortQueueSize})]),
  rpc:call(TraceNode, dbg, p, [all, [timestamp, c]]).
  %% start local:
  %dbg:tracer(Type, dbg:trace_port(ip, {TracePort, TracePortQueueSize})),
  %dbg:p(all, [timestamp, c]).

stop_dbg() ->
  ok.

stop() ->
  case whereis(?DBG_TRACE_CLIENT_NAME) of
    undefined ->
      io:format("trace_handler: The PID for ~p is undefined~n", [?DBG_TRACE_CLIENT_NAME]);
    Pid ->
      Pid ! stop
  end.

all_loaded() ->
  gen_server:call(?SERVER, all_loaded).
all_loaded_sorted() ->
  gen_server:call(?SERVER, all_loaded_sorted).

call(Request) ->
  gen_server:call(?SERVER, Request).

cast(Request) ->
  gen_server:cast(?SERVER, Request).

handle_trace(Trace, TraceCount) ->
  %io:format("~p:handle_trace(~p, ~p)~n", [?MODULE, handle_trace, TraceCount]),
  Request = {handle_trace, Trace, TraceCount},
  gen_server:cast(?SERVER, Request),
  TraceCount + 1.

handle_call(all_loaded, _From, State) ->
  io:format("~p~n", [State#state.all_loaded_modules]),
  {reply, ok, State};
handle_call(all_loaded_sorted, _From, State) ->
  io:format("~p~n", [dict:to_list(State#state.all_loaded_modules_sorted)]),
  {reply, ok, State};
handle_call(pretty_print_all_loaded_sorted, _From, State) ->
  DictToList = dict:to_list(State#state.all_loaded_modules_sorted),
  Fun1 = fun(E1) ->
    {Dir, ListOfModules} = E1,
    io:format("~p:~n", [Dir]),
    Fun2 = fun(E2) ->
      io:format("[111]~p~n", [E2])
    end,
    lists:foreach(Fun2, ListOfModules)
  end,
  lists:foreach(Fun1, DictToList),
  {reply, ok, State};
handle_call(Request, From, State) ->
  io:format("~p:handle_call: unexpected request ~p from ~p~n", [?SERVER, Request, From]),
  {reply, ok, State}.

handle_cast({enable_dbg, TraceFunction, Modules}, State) ->
  ListOfModules = enable_dbg(State#state.tracenode, Modules, [], TraceFunction),
  NewState = State#state{tpl = ListOfModules},
  {noreply, NewState};
handle_cast({handle_trace, end_of_trace, _TraceCount}, State) ->
  NewState = State,
  {noreply, NewState};
handle_cast({handle_trace, {drop, NoOfDroppedTraces}, TraceCount}, State) ->
  io:format("~p: ~p~n", [?MODULE, {drop, NoOfDroppedTraces}]),
  IoDevice = State#state.iodevice,
  Timestamp = "N/A",
  io:fwrite(IoDevice, "[~p]:~p ~p~n", [TraceCount, Timestamp, {drop, NoOfDroppedTraces}]),
  NewState = State,
  {noreply, NewState};
handle_cast({handle_trace, Trace, TraceCount}, State) ->
  IoDevice = State#state.iodevice,
  Timestamp =
        format_timestamp(element(1,Trace), element(tuple_size(Trace), Trace)),
  io:fwrite(IoDevice, "[~p]:~p ~p~n", [TraceCount, Timestamp, Trace]),
  NewState = State,
  dbgtracer_stats:trace_event(Trace),
  {noreply, NewState};
handle_cast({print_state, tpl}, State) ->
  io:format("~p~n", [State#state.tpl]),
  NewState = State,
  {noreply, NewState};
handle_cast({print_state, all_loaded_modules_sorted}, State) ->
  io:format("~p~n", [dict:to_list(State#state.all_loaded_modules_sorted)]),
  NewState = State,
  {noreply, NewState};
handle_cast({print_state, _}, State) ->
  io:format("~p~n", [State]),
  NewState = State,
  {noreply, NewState};
handle_cast({echo, Str}, State) ->
  io:format("~p~n", [Str]),
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

terminate(_Reason, State) ->
  file:close(State#state.iodevice),
  ok.
  
%%--------------------------------------------------------------------
%% Function: get_all_loaded_modules() -> ListOfModules
%%
%% ListOfModules = list()
%%   [{Path1, ListOfModules}, {Path2, ListOfModules}, ..]
%%
%% Description: Returns all modules in the system.
%%--------------------------------------------------------------------
get_all_loaded_modules() ->
  LoadedModules = code:all_loaded(),
  Fun = fun({Module, FilePath}, DictIn) ->
    Path = filename:dirname(FilePath),
    Key = list_to_atom(Path),
    DictOut = dict:append(Key, Module, DictIn),
    DictOut
  end,
  Dict = lists:foldl(Fun, dict:new(), LoadedModules),
  dict:to_list(Dict).
  
sort_loaded_modules(LoadedModules) ->
  Fun = fun({Module, FilePath}, DictIn) ->
    Path = filename:dirname(FilePath),
    Key = list_to_atom(Path),
    DictOut = dict:append(Key, Module, DictIn),
    DictOut
  end,
  Dict = lists:foldl(Fun, dict:new(), LoadedModules),
  %dict:to_list(Dict).
  Dict.
	
enable_dbg(_, [], ListOfActivatedModules, _TraceFunction) ->
  ListOfActivatedModules;
enable_dbg(TraceNode, [H|T], ListOfActivatedModules, TraceFunction) ->
  io:format("dbg:~p(~p, x)~n", [TraceFunction, H]),
  %Result = dbg:tpl(H, x),
  Result = rpc:call(TraceNode, dbg, TraceFunction, [H, x]),
  NewListOfActivatedModules =
    case Result of
      {ok, _} ->
        lists:append(ListOfActivatedModules, [H]);
      {badrpc, Reason} ->
        io:format("~p:~p failed for ~p with reason ~p~n", [?SERVER, enable_dbg_tpl, H, Reason]),
        ListOfActivatedModules
    end,
  enable_dbg(TraceNode, T, NewListOfActivatedModules, TraceFunction).

format_timestamp(trace_ts, Timestamp) ->
  {_, _, Micro} = Timestamp,
  {{Year,Month,Day},{Hour,Minute,Second}} =
    calendar:now_to_universal_time(Timestamp),
  % change timestamp format to YYYY-MM-DD HH:MM:SS.uuuuuu e.g. 2018-01-08 09:25:15.957492
  NewTimestamp =
    io_lib:format("~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~6..0w",
                  [Year, Month, Day, Hour, Minute, Second, Micro]),
  lists:flatten(NewTimestamp);
format_timestamp(_, _) ->
  "NO TIMESTAMP".