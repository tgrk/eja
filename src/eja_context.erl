%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for handling query parameters and context
%%% @end
%%%----------------------------------------------------------------------------
-module(eja_context).

%% API exports
-export([ create/1
        , create/2
        ]).

%%====================================================================
%% API functions
%%====================================================================

-spec create([tuple()]) -> map().
create(QueryArgs) ->
  DefaultOpts = #{
      include_header => false
  },
  create(QueryArgs, DefaultOpts).

-spec create([tuple()], map()) -> map().
create(QueryArgs, Opts) ->
  parse_query_args(QueryArgs, #{opts => Opts}).

%%====================================================================
%% Internal functions
%%====================================================================

parse_query_args([], Acc) ->
  Acc;
parse_query_args([{<<"include">>, V} | T], Acc) ->
  parse_query_args(T, maps:put(include, split(V), Acc));
parse_query_args([{<<"sort">>, V} | T], Acc) ->
  parse_query_args(T, maps:put(sort, parse_sort(V), Acc));
parse_query_args([{<<"filter", _Rest/binary>> = K, V} | T], Acc) ->
  parse_query_args(T, append_query_args_array(filter, {K, V}, Acc));
parse_query_args([{<<"fields", _Rest/binary>> = K, V} | T], Acc) ->
  parse_query_args(T, append_query_args_array(fields, {K, V}, Acc));
parse_query_args([{<<"page", _Rest/binary>> = K, V} | T], Acc) ->
  parse_query_args(T, append_query_args_array(page, {K, V}, Acc));
parse_query_args([_ | T], Acc) ->
  parse_query_args(T, Acc).

append_query_args_array(Label, {K, V}, Map) ->
  Existing = maybe_create_map(Label, Map),
  Parsed   = parse_query_args_array(Label, K),
  maps:put(
    Label, maps:put(Parsed, split(Label, V), Existing), Map
  ).

parse_query_args_array(Label, FieldKey) ->
  Len = byte_size(atom_to_binary(Label, latin1)),
  binary:part(FieldKey, Len + 1, byte_size(FieldKey) - (Len + 2)).

parse_sort([]) -> [];
parse_sort(Sort) ->
  [build_sort(S) || S <- split(Sort)].

build_sort(<<"-", (Field)/binary>>) -> {desc, Field};
build_sort(Field) -> {asc, Field}.

has_relationship(Type, Id, Context) ->
  %TODO: apply include/fields from query
  %TODO: return 400 if not applicable
  ok.

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
