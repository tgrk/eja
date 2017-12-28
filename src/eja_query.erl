%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for handling query parameters
%%% @end
%%%----------------------------------------------------------------------------
-module(eja_query).

%% API exports
-export([parse/1]).

%%====================================================================
%% API functions
%%====================================================================

parse(Args) when is_list(Args) ->
  parse(maps:from_list(Args));
parse(Args) when is_map(Args) ->
  Fields     = maps:get(<<"fields">>, Args, []),
  Include    = maps:get(<<"include">>, Args, []),
  Sort       = maps:get(<<"sort">>, Args, []),
  Filter     = maps:get(<<"filter">>, Args, []),
  Pagination = maps:get(<<"page">>, Args, []),
  #{fields  => parse_fields(Fields),
    include => parse_include(Include),
    sort    => parse_sort(Sort),
    filter  => parse_filter(Filter),
    page    => parse_pagination(Pagination)
  }.

%%====================================================================
%% Internal functions
%%====================================================================

parse_fields([]) -> [];
parse_fields(Fields) ->
  split(Fields).

parse_include([]) -> [].

parse_filter([]) -> [].

parse_sort([]) -> [];
parse_sort(Sort) ->
  [build_sort(S) || S <- split(Sort)].

parse_pagination([]) -> [];
parse_pagination(Pagination) ->
  #{<<"page">> => 0, <<"limit">> => 0}.

build_sort(<<"-", (Field)/binary>>) -> {desc, Field};
build_sort(Field) -> {asc, Field}.

split(Value) ->
  binary:split(Value, [<<",">>], [global]).
