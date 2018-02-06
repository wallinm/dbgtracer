-module(dbgtracer_api).

-export([print_tpl/0]).
-export([print_loaded_modules_sorted/0]).

-export([enable_tpl/1]).

print_tpl() ->
    dbgtracer_engine:cast({print_state, tpl}).

print_loaded_modules_sorted() ->
    dbgtracer_engine:cast({print_state, all_loaded_modules_sorted}).

enable_tpl(Modules) ->
  dbgtracer_engine:cast({tpl, Modules}).