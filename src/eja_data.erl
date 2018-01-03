%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for transforming data for building response object
%%% @end
%%%----------------------------------------------------------------------------
-module(eja_data).

%% API exports
-export([build/2]).

-define(QUERY_FIELDS, [fields, filter]).

%%====================================================================
%% API functions
%%====================================================================

%TODO: pagination - auto add -  "meta": {"total-pages": 13} info
%TODO: handle relationships on url level eg. /articles/1/relationships/comments
%TODO: fields vs relationships  eg. /articles?include=author&fields[articles]=title,body&fields[people]=name vs /articles?include=author&fields[articles]=title,body,author&fields[people]=name

-spec build([map()], map()) -> {ok, [map()]}.
build(Data, Context) ->
  QueryFuns = build_query_funs(Context),
  apply_row(Data, QueryFuns, []).

%%====================================================================
%% Internal functions
%%====================================================================

%%TODO: serialize types (datetime)
apply_row([], _QueryFuns, Acc) ->
  {ok, lists:reverse(Acc)};
apply_row([Row | Rows], QueryFuns, Acc) ->
  case apply_query(QueryFuns, Row) of
    skip ->
      apply_row(Rows, QueryFuns, Acc);
    ChangedRow ->
      apply_row(Rows, QueryFuns, [ChangedRow | Acc])
  end.

apply_query([], Row) ->
  Row;
apply_query([Fun | Funs], Row) ->
  case Fun(Row) of
      skip ->
        skip;
      ChangedRow ->
        apply_query(Funs, ChangedRow)
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

query_fun(fields, WithFields) ->
  fun (RowMap) -> maps:with(WithFields, RowMap) end;
query_fun(filter, FiltersMap) ->
  fun (RowMap) ->
      FilterPred = build_filter_pred(RowMap),
      case lists:all(FilterPred, maps:to_list(FiltersMap)) of
        true  -> RowMap;
        false -> skip
      end
  end;
query_fun(_Unknown, _Value) ->
  fun (RowMap) -> RowMap end.

build_filter_pred(RowMap) ->
  fun ({K, V}) when is_function(V) -> V(maps:get(K, RowMap));
      ({K, V}) -> not maps:is_key(K, RowMap) orelse maps:get(K, RowMap) == V
  end.

query_param(Key, Query) ->
  maps:get(Key, Query, []).