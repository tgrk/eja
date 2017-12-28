%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for building error response
%%% @end
%%%----------------------------------------------------------------------------
-module(eja_error).

%% API exports
-export([build/1]).

%%====================================================================
%% API functions
%%====================================================================

build(Errors) ->
  #{<<"errors">> => [build_error(E) || E <- Errors]}.

%%====================================================================
%% Internal functions
%%====================================================================

build_error(Error) when is_map(Error) ->
  drop_empty(
    build_error(
      {
        maps:get(title,  Error, <<"Unknown">>)
      , maps:get(status, Error, <<>>)
      , maps:get(detail, Error, <<>>)
      , maps:get(params, Error, #{})
      }
    )
  );

build_error({Title, Detail}) ->
  build_error({Title, <<>>, Detail});
build_error({Title, Status, Detail}) ->
  build_error({Title, Status, Detail, #{}});
build_error({Title, Status, Detail, Params}) ->
  maybe_append_fields(
    #{  <<"title">>  => Title
      , <<"detail">> => Detail
      , <<"status">> => Status
    },
    Params
  ).

drop_empty(ErrorObject) ->
  maps:filter(fun (_K, <<>>) -> false;
                  (_K, undefined) -> false;
                  (_K, _V) -> true end, ErrorObject).

maybe_append_fields(Error, Params) ->
  maps:fold(fun (K, V, Acc) -> append_field(K, V, Acc) end, Error, Params).

append_field(status, Value, Error) ->
  maps:put(<<"status">>, Value, Error);
append_field(meta, Value, Error) ->
  maps:put(<<"meta">>, #{<<"meta">> => Value}, Error);
append_field(source, Value, Error) when is_map(Value) ->
  maps:put(<<"source">>, Value, Error);
append_field(source, Value, Error) ->
  maps:put(<<"source">>, #{<<"pointer">> => Value}, Error);
append_field(_Field, _Value, Error) ->
    Error.