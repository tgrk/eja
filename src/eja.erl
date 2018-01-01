-module(eja).

%% API exports
-export([ include_api_version/1
        , get_header/1
        , validate_request_headers/1
        , create/3
        ]).

-define(JSONAPI_MIME_TYPE, "application/vnd.api+json").

%%====================================================================
%% API functions
%%====================================================================

-spec include_api_version(map()) -> map().
include_api_version(ResponseMap) when is_map(ResponseMap) ->
  maps:put(<<"jsonapi">>, #{<<"version">> => <<"1.0">>}, ResponseMap).

-spec get_header(atom()) -> tuple().
get_header(content_type) ->
  {"Content-Type", ?JSONAPI_MIME_TYPE};
get_header(accept) ->
  {"Accept", ?JSONAPI_MIME_TYPE}.

-spec validate_request_headers([tuple()]) ->
  ok | {error, unsupported_media_type} | {error, not_acceptable}.
validate_request_headers(Headers1) ->
  Headers = normalize_headers(Headers1, []),
  case has_valid_content_type_header(Headers) of
    true ->
      case has_valid_accept_header(Headers) of
        true ->
          ok;
        false ->
          {error, unsupported_media_type}
      end;
    false ->
      {error, not_acceptable}
  end.

-spec create(binary(), [map()] | map(), [tuple()]) -> map().
create(Type, Data, QueryArgs) ->
  Query = eja_query:parse(QueryArgs),
  case eja_data:apply(Data, Query) of
    {ok, ResponseData} ->
      eja_response:build(Type, ResponseData, Query);
    {error, Errors} ->
      eja_error:build(Errors)
  end.

%%====================================================================
%% Internal functions
%%====================================================================

has_valid_accept_header(Headers) ->
  case proplists:get_value("accept", Headers) of
    undefined ->
      false;
    Accept ->
      Accepts = string:split(Accept, ";"),
      lists:member(?JSONAPI_MIME_TYPE, Accepts)
        orelse lists:member("*/*", Accepts)
  end.

has_valid_content_type_header(Headers) ->
  case proplists:get_value("content-type", Headers) of
      undefined ->
        false;
      Value ->
        case string:split(Value, ";") of
          [?JSONAPI_MIME_TYPE] -> true;
          [?JSONAPI_MIME_TYPE | _] -> true;
          _Other -> false
        end
  end.

normalize_headers([], Acc) -> Acc;
normalize_headers([{K, V} | T], Acc) ->
  normalize_headers(T, [{string:lowercase(K), V} | Acc]).
