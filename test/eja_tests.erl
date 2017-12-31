-module(eja_tests).

-export([eja_test_/0]).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================

eja_test_() ->
  {setup,
    fun() -> ok end,
    fun(_) -> ok end,
    [
        {"Content Negotiation", fun test_content_negotiation/0}
      , {"Query Parser",        fun test_query_parser/0}
      , {"Data handling",       fun test_data_handling/0}
      , {"Response Object",     fun test_response_object/0}
      , {"Error Object",        fun test_error_object/0}
      , {"Relationships",       fun test_relationships/0}
      , {"Pagination",          fun test_pagination/0}
    ]
  }.

%% =============================================================================

test_content_negotiation() ->
  MakeHeaders = fun (CT, Accept) ->
    [ {"content-type", CT}
    , {"accept", Accept}
    ]
  end,

  ?assertEqual(
    {"Content-Type", "application/vnd.api+json"},
    eja:get_header(content_type)
  ),
  ?assertEqual(
    {"Accept", "application/vnd.api+json"},
    eja:get_header(accept)
  ),

  ?assertEqual(
    ok,
    eja:validate_request_headers(
      MakeHeaders("application/vnd.api+json", "application/vnd.api+json")
    )
  ),
  ?assertEqual(
    ok,
    eja:validate_request_headers(
      MakeHeaders("application/vnd.api+json", "*/*")
    )
  ),

  ?assertEqual(
    {error, not_acceptable},
    eja:validate_request_headers([])
  ),
  ?assertEqual(
    {error, unsupported_media_type},
    eja:validate_request_headers(
      MakeHeaders("application/vnd.api+json", "application/json")
    )
  ),
  ?assertEqual(
    {error, not_acceptable},
    eja:validate_request_headers(
      MakeHeaders("application/json", "application/vnd.api+json")
    )
  ),

  ok.

test_query_parser() ->
  Args = [
      {<<"include">>, <<"author">>}
    , {<<"fields[articles]">>, <<"title,body,author">>}
    , {<<"fields[people]">>, <<"name">>}
    , {<<"filter[tag]">>, <<"1,2">>}
    , {<<"sort">>, <<"title, author">>}
    , {<<"page[offset]">>, <<"1">>}
    , {<<"page[limit]">>, <<"15">>}
  ],
  Query = eja_query:parse(Args),
  ?assertEqual(
    #{  <<"articles">> => [<<"title">>,<<"body">>,<<"author">>]
      , <<"people">>   => [<<"name">>]
    },
    maps:get(fields, Query)
  ),
  ?assertEqual([<<"author">>], maps:get(include, Query)),
  ?assertEqual(
    #{<<"tag">> => [<<"1">>, <<"2">>]}
    , maps:get(filter, Query)
  ),
  ?assertEqual(
      [{asc, <<"title">>}, {asc, <<"author">>}]
    , maps:get(sort, Query)
  ),
  ?assertEqual(
    #{  <<"offset">> => <<"1">>
      , <<"limit">>  => <<"15">>
    },
    maps:get(page, Query)
  ),
  ok.

test_data_handling() ->
  Query = #{
      fields => [<<"title">>]
    , filter => #{<<"title">> => <<"Foo">>}
  },

  ?assertEqual(
    {ok, [#{<<"title">> => <<"Foo">>}]},
    eja_data:apply(
      Query,
      make_data()
    )
  ).

test_response_object() ->
  %% api version
  ?assertEqual(
    #{<<"version">> => <<"1.0">>},
    maps:get(<<"jsonapi">>, eja:include_api_version(maps:new()))
  ),

  %% documents
  Response = eja_response:build(
    <<"article">>,
    make_data(),
    #{fields => [<<"title">>]}
  ),

  [FirstRow, _] = Data = maps:get(<<"data">>, Response),
  ?assert(length(Data) == 2),
  ?assertEqual(
    maps:get(<<"id">>, FirstRow),
    <<"823ec82a-5e73-4013-a253-a2abf771c6db">>
  ),
  ?assertEqual(
    maps:get(<<"type">>, FirstRow),
    <<"article">>
  ),
  ?assertEqual(
    nested:get([<<"attributes">>, <<"title">>], FirstRow),
    <<"Foo">>
  ),
  ok.

test_error_object() ->
  Error1 = eja_error:build(
    [
       {<<"Bad Request">>, <<"42">>, <<"Missing foobar value!">>}
    , #{  title => <<"Bad Request">>
        , status => <<"42">>
        , detail => <<"Missing foobar value!">>
      }
    , #{  title => <<"Bad Request">>
        , detail => <<"Missing foobar value!">>
      }
  ]),
  [First, Two, Three] = Data = maps:get(<<"errors">>, Error1),
  ?assert(length(Data) == 3),
  ?assertEqual(
    #{  <<"detail">> => <<"Missing foobar value!">>
      , <<"status">> => <<"42">>
      , <<"title">> => <<"Bad Request">>
    },
    First
  ),
  ?assertEqual(First, Two),
  ?assertEqual(
    #{  <<"detail">> => <<"Missing foobar value!">>
      , <<"title">> => <<"Bad Request">>
    },
    Three
  ),
  ok.

test_relationships() ->
  ?assert(false).

test_pagination() ->
  ?assert(false).

%% =============================================================================

make_data() ->
  [
    #{  <<"id">> => <<"823ec82a-5e73-4013-a253-a2abf771c6db">>
      , <<"title">> => <<"Foo">>
      , <<"body">> => <<"Lorem impsum1">>
    },
    #{  <<"id">> => <<"309dcbed-e0b3-4cb5-b90a-9af7668556e2">>
      , <<"title">> => <<"Bar">>
      , <<"body">> => <<"Lorem impsum2">>
    }
  ].