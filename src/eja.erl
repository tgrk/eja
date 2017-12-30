-module(eja).

%% API exports
-export([ include_api_version/1
        , validate_request_headers/1
        ]).

-define(CONTENT_TYPE, "application/vnd.api+json").

%%====================================================================
%% API functions
%%====================================================================

include_api_version(ResponseMap) when is_map(ResponseMap) ->
  maps:put(<<"jsonapi">>, #{<<"version">> => <<"1.0">>}, ResponseMap).

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

%%====================================================================
%% Internal functions
%%====================================================================

has_valid_accept_header(Headers) ->
  case proplists:get_value("accept", Headers) of
    undefined ->
      false;
    Accept ->
      Accepts = string:split(Accept, ";"),
      lists:member(?CONTENT_TYPE, Accepts) orelse lists:member("*/*", Accepts)
  end.

has_valid_content_type_header(Headers) ->
  case proplists:get_value("content-type", Headers) of
      undefined ->
        false;
      Value ->
        case string:split(Value, ";") of
          [?CONTENT_TYPE] -> true;
          [?CONTENT_TYPE | _] -> true;
          _Other -> false
        end
  end.

normalize_headers([], Acc) -> Acc;
normalize_headers([{K, V} | T], Acc) ->
  normalize_headers(T, [{string:lowercase(K), V} | Acc]).
