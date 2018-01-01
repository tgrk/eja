%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for building pagination response
%%% @end
%%%----------------------------------------------------------------------------
-module(eja_pagination).

%% API exports
-export([build/1]).

-define(LINK_TARGETS, [
    <<"self">>
  , <<"first">>
  , <<"prev">>
  , <<"next">>
  , <<"last">>
  ]).

%%====================================================================
%% API functions
%%====================================================================

-spec build(map()) -> map().
build(Params) ->
  Uri     = maps:get(resource_uri, Params, <<>>),
  Page    = maps:get(page, Params, #{}),
  Total   = maps:get(total, Params, 1),

  Current = maps:get(<<"number">>, Page, 1),
  Size    = maps:get(<<"size">>, Page, 25),
  Last    = round(math:floor(Total / Size)),

  #{<<"links">> =>
    build_links(?LINK_TARGETS, Uri, Current, Last, Size, #{})
  }.

%%====================================================================
%% Internal functions
%%====================================================================

build_links([], _Uri, _Current, _Last, _Size, Acc) ->
  Acc;
build_links([Type | Types], Uri, Current, Last, Size, Acc) ->
  Acc1 = build_link(Type, Uri, Current, Last, Size, Acc),
  build_links(Types, Uri, Current, Last, Size, Acc1).

build_link(<<"self">>, Uri, Current, _Last, Size, Acc) ->
  maps:put(<<"self">>, build_uri(Uri, Current, Size), Acc);
build_link(<<"first">>, Uri, _Current, _Last, Size, Acc) ->
  maps:put(<<"first">>, build_uri(Uri, 1, Size), Acc);
build_link(<<"prev">>, Uri, Current, _Last, Size, Acc) ->
  Prev = Current - 1,
  case Prev > 1 of
    true ->
      maps:put(<<"prev">>, build_uri(Uri, Prev, Size), Acc);
    false ->
      Acc
  end;
build_link(<<"next">>, Uri, Current, Last, Size, Acc) ->
  Next = Current + 1,
  case Next < Last of
    true ->
      maps:put(<<"next">>, build_uri(Uri, Next, Size), Acc);
    false ->
      Acc
  end;
build_link(<<"last">>, Uri, _Current, Last, Size, Acc) ->
  maps:put(<<"last">>, build_uri(Uri, Last, Size), Acc).

build_uri(BaseUri, Number, Size) ->
  list_to_binary(
    BaseUri ++ "?page[number]=" ++ integer_to_list(Number)
      ++ "&page[size]=" ++ integer_to_list(Size)
  ).