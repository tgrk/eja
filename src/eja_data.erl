%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for transforming data for building response object
%%% @end
%%%----------------------------------------------------------------------------
-module(eja_data).

-compile({no_auto_import, [apply/2]}).

%% API exports
-export([apply/2]).

-define(QUERY_FIELDS, [fields, filter]).

%%====================================================================
%% API functions
%%====================================================================

apply(Query, Data) ->
  QueryFuns = build_query_funs(Query),
  apply_row(Data, QueryFuns, []).

%%====================================================================
%% Internal functions
%%====================================================================

apply_row([], _QueryFuns, Acc) ->
  lists:reverse(Acc);
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
      FilterPred = fun ({K, V}) when is_function(V) -> V(maps:get(K, RowMap));
                       ({K, V}) -> not maps:is_key(K, RowMap) orelse maps:get(K, RowMap) == V
                    end,
      case lists:all(FilterPred, maps:to_list(FiltersMap)) of
        true  -> RowMap;
        false -> skip
      end
  end;
query_fun(_Unknown, _Value) ->
  fun (RowMap) -> RowMap end.

query_param(Key, Query) ->
  maps:get(Key, Query, []).