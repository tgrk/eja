%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for building response object
%%% @end
%%%----------------------------------------------------------------------------
-module(eja_response).

%% API exports
-export([ serialize/3
        , deserialize/1
        , validate/1
        ]).

-define(REQUIRED_FIELDS, [
    <<"id">>
  , <<"type">>
  , <<"attributes">>
]).

%%====================================================================
%% API functions
%%====================================================================

-spec serialize(binary(), [map()] | map(), map()) -> map().
serialize(Type, Data, Context) ->
  WithFields = maps:get(fields, Context, []),
  Opts       = maps:get(opts, Context, #{}),
  IncludeHeader = maps:get(include_header, Opts, false),
  build_response(
    build_data(Type, Data, WithFields, Opts),
    Context,
    IncludeHeader
  ).

-spec deserialize([map()]) -> [map()].
deserialize(Data) ->
  [from_object(R) || R <- Data].

-spec validate(map()) -> ok | {error, bad_request}.
validate(Data) ->
  case maps:get(<<"data">>, Data, false) of
    false ->
      {error, bad_reques};
    [] ->
      ok;
    DataItems ->
      validate_data_items(DataItems)
  end.

%%====================================================================
%% Internal functions
%%====================================================================

validate_data_items([]) ->
  ok;
validate_data_items([H | T]) ->
  case maps:take(?REQUIRED_FIELDS, H) of
    error ->
      {error, bad_request};
    _Found ->
      validate_data_items(T)
  end.

from_object(ResponseObject) ->
  %%TODO: process types
  Id = maps:get(<<"id">>, ResponseObject),
  Attributes = maps:get(<<"attributes">>, ResponseObject),
  maps:put(<<"id">>, Id, Attributes).

build_response(Data, Context, IncludeHeader) ->
  Object = #{
    <<"data">>          => Data,
    <<"meta">>          => maps:get(meta, Context, []),
    <<"links">>         => maps:get(links, Context, []),
    <<"included">>      => maps:get(included, Context, []),
    <<"relationships">> => maps:get(relationships, Context, [])
  },
  maps:filter(
    fun filter_empty/2,
    maybe_include_header(IncludeHeader, Object)
  ).

build_data(Type, Data, WithFields, Opts) when is_map(Data) ->
  [build_data_item(Type, Data, WithFields, Opts)];
build_data(Type, Data, WithFields, Opts) when is_list(Data) ->
  [build_data_item(Type, Item, WithFields, Opts) || Item <- Data].

build_data_item(Type, Item, WithFields, Opts) ->
  EnforceUnderscores = maps:get(enforce_underscores, Opts, false),
  #{<<"id">>         => maps:get(<<"id">>, Item, create_default_id()),
    <<"type">>       => Type,
    <<"attributes">> => maybe_filter_attributes(
                          Item, WithFields, EnforceUnderscores
                        )
  }.

create_default_id() ->
  list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

maybe_filter_attributes(Item, [], EnforceUnderscores) ->
  filter_defaults(Item, EnforceUnderscores);
maybe_filter_attributes(Item, WithFields, EnforceUnderscores) ->
  filter_defaults(maps:with(WithFields, Item), EnforceUnderscores).

filter_empty(<<"data">>, _Value) -> true;
filter_empty(_Key, []) -> false;
filter_empty(_Key, _Value) -> true.

filter_defaults(Item, false) ->
  maps:without([<<"id">>], Item);
filter_defaults(Item, true) ->
  enforce_underscores(maps:without([<<"id">>], Item)).

enforce_underscores(Attributes) ->
  maps:fold(fun (K, V, Acc) ->
    maps:put(enforce_underscore(K), V, Acc)
  end, #{}, Attributes).

enforce_underscore(Val) ->
  binary:replace(Val, [<<"-">>], <<"_">>, [global]).

maybe_include_header(true, Object) ->
  maps:put(<<"jsonapi">>, #{<<"version">> => <<"1.0">>}, Object);
maybe_include_header(false, Object) ->
  Object.