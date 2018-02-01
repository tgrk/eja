-module(eja).

%% API exports
-export([ create/3
        , create/4

        , validate_request_headers/1
        , validate_payload/1
        , validate_query/1

        , get_header/1
        ]).

-define(JSONAPI_MIME_TYPE, "application/vnd.api+json").

%%====================================================================
%% API functions
%%====================================================================

-spec create(binary(), [map()] | map(), [tuple()]) -> map().
create(Type, Data, QueryArgs) ->
  handle(Type, Data, eja_context:create(QueryArgs)).

-spec create(binary(), [map()] | map(), [tuple()], map()) -> map().
create(Type, Data, QueryArgs, Opts) ->
  handle(Type, Data, eja_context:create(QueryArgs, Opts)).

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

-spec validate_payload(map()) -> ok | {error, bad_request}.
validate_payload(Data) when is_map(Data) ->
  eja_response:validate(Data);
validate_payload(_Data) ->
  {error, bad_request}.

validate_query(_Args) ->
  not_implemented.

%%====================================================================
%% Internal functions
%%====================================================================

handle(Type, Data, Context) ->
  case eja_data:build(Type, Data, Context) of
    {ok, ResponseData} ->
      case eja_response:serialize(Type, ResponseData, Context) of
        {ok, Response} ->
          Response;
        {error, {wrong_type, _Type}} = Error ->
          Error
      end;
    {error, Errors} ->
      eja_error:serialize(Errors)
  end.

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
