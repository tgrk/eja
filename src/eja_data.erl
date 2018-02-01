%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for transforming data for building response object
%%% @end
%%%----------------------------------------------------------------------------
-module(eja_data).

%% API exports
-export([ build/3
        , extract_type/1
        ]).

-define(QUERY_FIELDS, [fields, filter]).

%%====================================================================
%% API functions
%%====================================================================

%TODO: handle relationships on url level eg. /articles/1/relationships/comments
%TODO: fields vs relationships  eg. /articles?include=author&fields[articles]=title,body&fields[people]=name vs /articles?include=author&fields[articles]=title,body,author&fields[people]=name

-spec build(binary(), map(), map()) -> {ok, map()}.
build(Type, Data, Context) ->
  QueryFuns = build_query_funs(Context),
  MainData  = maps:get(Type, Data, []),
  Output    = apply_row(MainData, Type, Data, QueryFuns, []),
  {ok, maps:put(Type, Output, #{})}.

-spec extract_type(map()) -> map().
extract_type(_DataItem) ->
  not_implemented.

%%====================================================================
%% Internal functions
%%====================================================================

%%TODO: serialize types (datetime)
apply_row([], _Type, _Data, _QueryFuns, Acc) ->
  lists:reverse(Acc);
apply_row([Row | Rows], Type, Data, QueryFuns, Acc) ->
  case apply_query(QueryFuns, Row, Type, Data) of
    skip ->
      apply_row(Rows, Type, Data, QueryFuns, Acc);
    ChangedRow ->
      apply_row(Rows, Type, Data, QueryFuns, [ChangedRow | Acc])
  end.

apply_query([], Row, _Type, _Data) ->
  Row;
apply_query([Fun | Funs], Row, Type, Data) ->
  case Fun(Row, Type, Data) of
      skip ->
        skip;
      ChangedRow ->
        apply_query(Funs, ChangedRow, Type, Data)
  end.

build_query_funs(Query) ->
  build_query_funs(?QUERY_FIELDS, Query, []).

build_query_funs([], _Query, Acc) ->
  Acc;
build_query_funs([Key | Keys], Query, Acc) ->
  case query_param(Key, Query) of
    [] ->
      build_query_funs(Keys, Query, Acc);
    Value ->
      build_query_funs(Keys, Query, [query_fun(Key, Value) | Acc])
  end.

query_fun(fields, TypesWithFields) ->
  fun (RowMap, Type, _Data) ->
    %%TODO: apply sparese fields on relatations/includes form Data (if present)
    %% sparse fields are stored per type
    WithFields = maps:get(Type, TypesWithFields, []),
    maps:with(WithFields, RowMap)
  end;
query_fun(filter, FiltersMap) ->
  fun (RowMap, Type, _Data) ->
      %%TODO: apply filters on relatations/includes form Data (if present)
      FilterPred   = build_filter_pred(RowMap),
      FilterFields = maps:get(Type, FiltersMap),
      case lists:all(FilterPred, maps:to_list(FilterFields)) of
        true  -> RowMap;
        false -> skip
      end
  end;
query_fun(_Unknown, _Value) ->
  fun (RowMap, _Type, _Data) -> RowMap end.

build_filter_pred(RowMap) ->
  fun ({K, V}) when is_function(V) ->
        V(maps:get(K, RowMap));
      ({K, V}) when is_list(V) ->
        not maps:is_key(K, RowMap) orelse lists:member(maps:get(K, RowMap), V);
      ({K, V}) ->
        not maps:is_key(K, RowMap) orelse maps:get(K, RowMap) == V
  end.

query_param(Key, Query) ->
  maps:get(Key, Query, []).
