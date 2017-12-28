-module(eja).

%% API exports
-export([get_header/0]).

%%====================================================================
%% API functions
%%====================================================================

get_header() ->
  {"Content-Type", "application/vnd.api+json"}.

%%====================================================================
%% Internal functions
%%====================================================================
