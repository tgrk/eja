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

-spec serialize(binary(), map(), map()) -> {ok, map()} | {error, term()}.
serialize(Type, Data, Context0) ->
  Context    = apply_opts_funs(Context0),
  WithFields = maps:get(Type, maps:get(fields, Context, #{}), []),
  Opts       = maps:get(opts, Context, #{}),

  %%TODO: how to handle relations as we now deal only with top level
  %%      types
  case maps:get(Type, Data, wrong_type) of
    wrong_type ->
      {error, {wrong_type, Type}};
    Rows ->
      build_response(
        build_data(Type, Rows, WithFields, Opts),
        Context,
        maps:get(include_header, Opts, false)
      )
end.

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
  Response = maps:filter(
    fun filter_empty/2,
    maybe_include_header(IncludeHeader, Object)
  ),
  {ok, Response}.

build_data(Type, Data, WithFields, Opts) when is_map(Data) ->
  [build_data_item(Type, Data, WithFields, Opts)];
build_data(Type, Data, WithFields, Opts) when is_list(Data) ->
  [build_data_item(Type, Row, WithFields, Opts) || Row <- Data].

build_data_item(Type, Row, WithFields, Opts) ->
  EnforceUnderscores =  maps:get(enforce_underscores, Opts, false),
  #{<<"id">>         => maps:get(<<"id">>, Row, create_default_id()),
    <<"type">>       => Type,
    <<"attributes">> => maybe_filter_attributes(
                          Row, WithFields, EnforceUnderscores
                        )
  }.

create_default_id() ->
  list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

maybe_filter_attributes(Row, [], EnforceUnderscores) ->
  filter_defaults(Row, EnforceUnderscores);
maybe_filter_attributes(Row, WithFields, EnforceUnderscores) ->
  filter_defaults(maps:with(WithFields, Row), EnforceUnderscores).

filter_empty(<<"data">>, _Value) -> true;
filter_empty(_Key, []) -> false;
filter_empty(_Key, _Value) -> true.

filter_defaults(Row, false) ->
  maps:without([<<"id">>], Row);
filter_defaults(Row, true) ->
  enforce_underscores(maps:without([<<"id">>], Row)).

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
