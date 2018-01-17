%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for building error response
%%% @end
%%%----------------------------------------------------------------------------
-module(eja_error).

%% API exports
-export([serialize/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec serialize([map()]) -> map().
serialize(Errors) ->
  #{<<"errors">> => [drop_empty(build_error(E)) || E <- Errors]}.

%%====================================================================
%% Internal functions
%%====================================================================

build_error(Error) when is_map(Error) ->
  build_error(
    { maps:get(title,  Error, <<"Unknown">>)
    , maps:get(status, Error, <<>>)
    , maps:get(detail, Error, <<>>)
    , maps:get(params, Error, #{})
    }
  );
build_error({Title, Detail}) ->
  build_error({Title, <<>>, Detail});
build_error({Title, Status, Detail}) ->
  build_error({Title, Status, Detail, #{}});
build_error({Title, Status, Detail, Params}) ->
  maybe_append_params(
    #{  <<"title">>  => Title
      , <<"detail">> => Detail
      , <<"status">> => Status
    },
    Params
  ).

drop_empty(ErrorObject) ->
  maps:filter(fun (_K, <<>>) -> false;
                  (_K, undefined) -> false;
                  (_K, _V) -> true
              end, ErrorObject).

maybe_append_params(Error, Params) ->
  maps:fold(fun (K, V, Acc) -> append_params(K, V, Acc) end, Error, Params).

append_params(meta, Value, Error) when is_map(Value) ->
  maps:put(<<"meta">>, #{<<"meta">> => ensure_binary(Value)}, Error);
append_params(source, Value, Error) when is_map(Value) ->
  maps:put(<<"source">>, ensure_binary(Value), Error);
append_params(source, Value, Error) when is_binary(Value) ->
  maps:put(<<"source">>, #{<<"pointer">> => Value}, Error);
append_params(_Field, _Value, Error) ->
  Error.

ensure_binary(Map) when is_map(Map) ->
  maps:fold(fun (K, V, Acc) when is_atom(K) ->
                  maps:put(atom_to_binary(K, latin1), V, Acc);
                (K, V, Acc) when is_list(K) ->
                  maps:put(list_to_binary(K), V, Acc);
                (K, V, Acc) ->
                  maps:put(K, V, Acc)
            end, #{}, Map).
