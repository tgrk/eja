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

-define(REQUIRED_FIELDS, sets:from_list([
      <<"attributes">>
    , <<"id">>
    , <<"type">>
    ])
  ).

%%====================================================================
%% API functions
%%====================================================================

-spec serialize(binary(), [map()] | map(), map()) -> map().
serialize(Type, Data, Context0) ->
  Context    = apply_opts_funs(Context0),
  WithFields = maps:get(fields, Context, []),
  Opts       = maps:get(opts, Context, #{}),
  build_response(
    build_data(Type, Data, WithFields, Opts),
    Context,
    maps:get(include_header, Opts, false)
  ).

-spec deserialize([map()]) -> [map()].
deserialize(Data) ->
  [from_object(R) || R <- Data].

-spec validate(map()) -> ok | {error, bad_request}.
validate(Payload) ->
  case maps:get(<<"data">>, Payload, false) of
    false ->
      {error, bad_request};
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
  case has_required_keys(H) of
    false ->
      {error, bad_request};
    true ->
      validate_data_items(T)
  end.

has_required_keys(Item) ->
  sets:is_subset(?REQUIRED_FIELDS, sets:from_list(maps:keys(Item))).

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

apply_opts_funs(Context) ->
  Opts = maps:get(opts, Context, #{}),
  Funs = [  fun maybe_build_pagination_meta/2
          , fun maybe_build_pagination/2
  ],
  lists:foldl(fun (Fun, Acc) -> Fun(Acc, Opts) end, Context, Funs).

maybe_build_pagination(Context, Opts) ->
  case maps:is_key(page, Opts) of
    false ->
      Context;
    true ->
      maybe_append(links, Context, eja_pagination:build_links(Opts))
  end.

maybe_build_pagination_meta(Context, Opts) ->
  case maps:get(total, Opts, false) of
    false ->
      Context;
    Total ->
      maybe_append(meta, Context, eja_pagination:build_meta(Total))
  end.

maybe_append(Key, Map, Value) ->
  case maps:get(Key, Map, false) of
    false ->
      maps:put(Key, Value, Map);
    Existing  ->
      maps:put(Key, maps:merge(Existing, Value), Map)
  end.
