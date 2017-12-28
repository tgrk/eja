%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for building response object
%%% @end
%%%----------------------------------------------------------------------------
-module(eja_response).

%% API exports
-export([build/3]).

%%====================================================================
%% API functions
%%====================================================================

build(Type, Data, Query) ->
  Meta          = maps:get(meta, Query, []),
  Links         = maps:get(links, Query, []),
  Included      = maps:get(included, Query, []),
  Relationships = maps:get(relationships, Query, []),
  WithFields    = maps:get(fields, Query, []),
  build_response(
    build_data(Type, Data, WithFields), Meta, Links, Included, Relationships
  ).

%%====================================================================
%% Internal functions
%%====================================================================

build_response(Data, Meta, Links, Included, Relationships) ->
  Object = #{
    <<"data">>          => Data,
    <<"meta">>          => Meta,
    <<"links">>         => Links,
    <<"included">>      => Included,
    <<"relationships">> => Relationships
  },
  maps:filter(fun filter_empty/2, Object).

build_data(Type, Data, WithFields) ->
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