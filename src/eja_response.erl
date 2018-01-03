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
        ]).

%%====================================================================
%% API functions
%%====================================================================

-spec serialize(binary(), [map()] | map(), map()) -> map().
serialize(Type, Data, Context) ->
  WithFields = maps:get(fields, Context, []),
  build_response(
    build_data(Type, Data, WithFields), Context
  ).

-spec deserialize([map()]) -> [map()].
deserialize(Data) ->
  [from_object(R) || R <- Data].

%%====================================================================
%% Internal functions
%%====================================================================

from_object(ResponseObject) ->
  %%TODO: process types
  Id = maps:get(<<"id">>, ResponseObject),
  Attributes = maps:get(<<"attributes">>, ResponseObject),
  maps:put(<<"id">>, Id, Attributes).

build_response(Data, Context) ->
  IncludeHeader = nested:get([opts, include_header], Context, false),
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

build_data(Type, Data, WithFields) when is_map(Data) ->
      [build_data_item(Type, Data, WithFields)];
build_data(Type, Data, WithFields) when is_list(Data) ->
  [build_data_item(Type, Item, WithFields) || Item <- Data].

build_data_item(Type, Item, WithFields) ->
  #{<<"id">>         => maps:get(<<"id">>, Item, create_default_id()),
    <<"type">>       => Type,
    <<"attributes">> => maybe_filter_attributes(Item, WithFields)
  }.

create_default_id() ->
  list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

maybe_filter_attributes(Item, []) ->
  filter_defaults(Item);
maybe_filter_attributes(Item, WithFields) ->
  filter_defaults(maps:with(WithFields, Item)).

filter_empty(<<"data">>, _Value) -> true;
filter_empty(_Key, []) -> false;
filter_empty(_Key, _Value) -> true.

filter_defaults(Item) ->
  maps:without([<<"id">>], Item).

maybe_include_header(true, Object) ->
  maps:put(<<"jsonapi">>, #{<<"version">> => <<"1.0">>}, Object);
maybe_include_header(false, Object) ->
  Object.