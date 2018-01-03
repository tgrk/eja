-module(eja).

%% API exports
-export([ create/3
        , get_header/1
        , validate_request_headers/1
        ]).

-define(JSONAPI_MIME_TYPE, "application/vnd.api+json").

%%====================================================================
%% API functions
%%====================================================================

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

%%TODO: allow to pass opts (optional)
-spec create(binary(), [map()] | map(), [tuple()]) -> map().
create(Type, Data, QueryArgs) ->
  Context = eja_context:create(QueryArgs),
  case eja_data:build(Data, Context) of
    {ok, ResponseData} ->
      eja_response:serialize(Type, ResponseData, Context);
    {error, Errors} ->
      eja_error:serialize(Errors)
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

%%TODO: generic type conversion (used on uri params and json data)
parse(number, Val) ->
  case re:run(Val, "^[0-9]*$") of
    nomatch ->
      {error, unable_to_parse};
    _Match ->
      {ok, binary_to_integer(Val)}
  end;
parse(date, Value) ->
  iso8601:parse(Value).
