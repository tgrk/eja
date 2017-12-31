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

-spec parse([tuple()]) -> map().
parse(Args) when is_list(Args) ->
  parse_args(Args, #{}).

%%====================================================================
%% Internal functions
%%====================================================================

parse_args([], Acc) ->
  Acc;
parse_args([{<<"include">>, V} | T], Acc) ->
  parse_args(T, maps:put(include, split(V), Acc));
parse_args([{<<"sort">>, V} | T], Acc) ->
  parse_args(T, maps:put(sort, parse_sort(V), Acc));
parse_args([{<<"filter", _Rest/binary>> = K, V} | T], Acc) ->
  parse_args(T, collect_array_args(filter, {K, V}, Acc));
parse_args([{<<"fields", _Rest/binary>> = K, V} | T], Acc) ->
  parse_args(T, collect_array_args(fields, {K, V}, Acc));
parse_args([{<<"page", _Rest/binary>> = K, V} | T], Acc) ->
  parse_args(T, collect_array_args(page, {K, V}, Acc));
parse_args([_ | T], Acc) ->
  parse_args(T, Acc).

collect_array_args(Label, {K, V}, Map) ->
  Existing = maybe_create_map(Label, Map),
  Parsed   = parse_args_array(Label, K),
  maps:put(
    Label, maps:put(Parsed, split(Label, V), Existing), Map
  ).

parse_args_array(Label, FieldKey) ->
  Len = byte_size(atom_to_binary(Label, latin1)),
  binary:part(FieldKey, Len + 1, byte_size(FieldKey) - (Len + 2)).

parse_sort([]) -> [];
parse_sort(Sort) ->
  [build_sort(S) || S <- split(Sort)].

build_sort(<<"-", (Field)/binary>>) -> {desc, Field};
build_sort(Field) -> {asc, Field}.

split(Value) ->
  binary:split(Value, [<<",">>, <<" ">>], [global, trim_all]).

split(Label, Value) ->
  case split(Value) of
    [One] when Label == page -> One;
    More -> More
  end.

maybe_create_map(Label, Map) ->
  case maps:get(Label, Map, undefined) of
    undefined -> #{};
    Existing  -> Existing
  end.
