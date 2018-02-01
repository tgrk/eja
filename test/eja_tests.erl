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
      , {"Context handling",    fun test_context_handling/0}
      , {"Data handling",       fun test_data_handling/0}
      , {"Enforce underscores", fun test_enforce_underscores/0}
      , {"Response Object",     fun test_response_object/0}
      , {"Error Object",        fun test_error_object/0}
      , {"Relationships",       fun test_relationships/0}
      , {"Pagination",          fun test_pagination/0}
      , {"Top level API",       fun test_top_api/0}
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

test_context_handling() ->
  Args = [
      {<<"include">>, <<"author">>}
    , {<<"fields[articles]">>, <<"title,body,author">>}
    , {<<"fields[people]">>, <<"name">>}
    , {<<"filter[tag]">>, <<"1,2">>}
    , {<<"sort">>, <<"title, author">>}
    , {<<"page[offset]">>, <<"1">>}
    , {<<"page[limit]">>, <<"15">>}
  ],
  Context = eja_context:create(Args),

  ?assertEqual(
    #{  <<"articles">> => [<<"title">>,<<"body">>,<<"author">>]
      , <<"people">>   => [<<"name">>]
    },
    maps:get(fields, Context)
  ),

  ?assertEqual([<<"author">>], maps:get(include, Context)),

  ?assertEqual(
    #{<<"tag">> => [<<"1">>, <<"2">>]}
    , maps:get(filter, Context)
  ),

  ?assertEqual(
      [{asc, <<"title">>}, {asc, <<"author">>}]
    , maps:get(sort, Context)
  ),

  ?assertNot(maps:is_key(page, Context)),
  ?assertEqual(
    #{  <<"offset">> => <<"1">>
      , <<"limit">>  => <<"15">>
    },
    nested:get([opts, page], Context)
  ),

  ?assertEqual(
    [{asc,<<"title">>},{asc,<<"author">>}],
    maps:get(sort, Context)
  ),

  %% context
  ?assertEqual(
    false,
    nested:get([opts, include_header], Context)
  ),

  ok.

test_data_handling() ->
  Query = #{
      fields => #{
        <<"articles">> => [<<"title">>]
      }
    , filter => #{
        <<"articles">> => #{
          <<"title">> => [<<"Foo1">>]
        }
      }
  },

  ?assertEqual(
    {ok, #{<<"articles">> => [
            #{<<"title">> => <<"Foo1">>}
          ]
        }
    },
    eja_data:build(<<"articles">>, make_data(), Query)
  ),

  ok.

test_enforce_underscores() ->
  Data = #{
    <<"posts">> =>
        [
          #{    <<"post-id">>    => <<"823ec82a-5e73-4013-a253-a2abf771c6db">>
              , <<"post-title">> => <<"Foo">>
              , <<"post-body">>  => <<"Lorem-impsum1">>
            }
        ]
  },
  Opts = #{opts => #{
    enforce_underscores => true}
  },

  {ok, Response} = eja_response:serialize(<<"posts">>, Data, Opts),
  Result = maps:get(<<"data">>, Response),

  ?assertEqual(
    #{<<"post_body">> => <<"Lorem-impsum1">>,
      <<"post_id">> => <<"823ec82a-5e73-4013-a253-a2abf771c6db">>,
      <<"post_title">> => <<"Foo">>
    },
    maps:get(<<"attributes">>, hd(Result))
  ).

test_response_object() ->
  InputData = make_data(),

  %% serialize basic document
  {ok, Response} = eja_response:serialize(
    <<"articles">>,
    InputData,
    #{  fields => #{<<"articles">> => [<<"title">>]}
      , opts   => #{include_header => true}
    }
  ),

  [FirstRow, _] = Data = maps:get(<<"data">>, Response),
  ?assertEqual(
    #{<<"version">> => <<"1.0">>},
    maps:get(<<"jsonapi">>, Response)
  ),
  ?assert(length(Data) == 2),

  ?assertEqual(
    maps:get(<<"id">>, FirstRow),
    <<"823ec82a-5e73-4013-a253-a2abf771c6db">>
  ),
  ?assertEqual(
    maps:get(<<"type">>, FirstRow),
    <<"articles">>
  ),
  ?assertEqual(
    nested:get([<<"attributes">>, <<"title">>], FirstRow),
    <<"Foo1">>
  ),

  %% deserialize basic document
  % ?assertEqual(
  %   maps:remove(<<"body">>, hd(InputData)),
  %   hd(eja_response:deserialize([FirstRow]))
  % ),

  ok.

test_error_object() ->
  Error1 = eja_error:serialize(
    [
       {<<"Bad Request">>, <<"42">>, <<"Missing foobar value!">>}
    ,  {<<"Bad Request">>, <<"Missing foobar value!">>}
    , #{
          title  => <<"Bad Request">>
        , status => <<"42">>
        , detail => <<"Missing foobar value!">>
      }
    , #{
          title  => <<"Bad Request">>
        , detail => <<"Missing foobar value!">>
      }
    , #{
          title  => <<"Bad Request">>
        , detail => <<"Missing foobar value!">>
        , params => #{
            source => #{pointer => <<"/path/field">>}
          }
      }
    , #{
          title  => <<"Invalid Query Parameter">>
        , detail => <<"The resource does not have an `author` relationship path.">>
        , params => #{
            source => #{parameter => <<"include">>}
       }
    }
  ]),
  [First, Two, Three, Four, Five, Six] = maps:get(<<"errors">>, Error1),
  ?assertEqual(
    #{  <<"detail">> => <<"Missing foobar value!">>
      , <<"status">> => <<"42">>
      , <<"title">>  => <<"Bad Request">>
    },
    First
  ),
  ?assertEqual(First, Three),
  ?assertEqual(Two, Four),
  ?assertEqual(
    #{  <<"detail">> => <<"Missing foobar value!">>
      , <<"title">>  => <<"Bad Request">>
    },
    Four
  ),
  ?assertEqual(
    #{  <<"detail">> => <<"Missing foobar value!">>
      , <<"source">> => #{<<"pointer">> => <<"/path/field">>}
      , <<"title">>  => <<"Bad Request">>
    },
    Five
  ),
  ?assertEqual(
    #{
        <<"detail">> => <<"The resource does not have an `author` relationship path.">>
      , <<"source">> => #{<<"parameter">> => <<"include">>}
      , <<"title">>  => <<"Invalid Query Parameter">>
    },
    Six
  ),

  ok.

test_relationships() ->
  ok.

test_pagination() ->
  TotalPages = 150,
  Data = make_data(TotalPages),

  ParseUriFun = fun(Type, Links) ->
                  case maps:get(Type, Links, undefined) of
                    undefined ->
                      missing;
                    Url ->
                      {ok, Uri} = http_uri:parse(Url),
                      element(6, Uri)
                  end
                end,

  PageSize = 25,
  Opts = #{
      resource_uri => "http://example.com/articles"
    , page         => #{<<"number">> => 3, <<"size">> => PageSize}
    , total        => TotalPages
  },
  Links = eja_pagination:build_links(Opts),

  ?assertEqual(
    <<"?page[number]=1&page[size]=25">>,
    ParseUriFun(<<"first">>, Links)
  ),
  ?assertEqual(
    <<"?page[number]=2&page[size]=25">>,
    ParseUriFun(<<"prev">>, Links)
  ),
  ?assertEqual(
    <<"?page[number]=3&page[size]=25">>,
    ParseUriFun(<<"self">>, Links)
  ),
  ?assertEqual(
    <<"?page[number]=4&page[size]=25">>,
    ParseUriFun(<<"next">>, Links)
  ),
  ?assertEqual(
    <<"?page[number]=6&page[size]=25">>,
    ParseUriFun(<<"last">>, Links)
  ),

  %%TODO: test edge cases

  Meta = eja_pagination:build_meta(TotalPages),
  ?assertEqual(
    TotalPages,
    maps:get(<<"total-pages">>, Meta)
  ),

  Data1 = maps:get(<<"articles">>, Data),
  {ok, Response} = eja_response:serialize(
    <<"articles">>,
    #{<<"articles">> => lists:sublist(Data1, PageSize)},
    #{  fields => #{<<"articles">> => [<<"title">>]}
      , opts   => Opts
    }
  ),
  ?assertEqual(
    [<<"data">>, <<"links">>, <<"meta">>],
    maps:keys(Response)
  ),

  ok.

test_top_api() ->
  Response1 = eja:create(<<"articles">>, make_data(), []),
  [FirstRow, _] = Data = maps:get(<<"data">>, Response1),

  ?assert(length(Data) == 2),
  ?assertEqual(
    maps:get(<<"id">>, FirstRow),
    <<"823ec82a-5e73-4013-a253-a2abf771c6db">>
  ),
  ?assertEqual(
    maps:get(<<"type">>, FirstRow),
    <<"articles">>
  ),
  ?assertEqual(
    nested:get([<<"attributes">>, <<"title">>], FirstRow),
    <<"Foo1">>
  ),

  % validations
  ?assertEqual(
    ok,
    eja:validate_payload(#{<<"data">> => []})
  ),
  {ok, Response} = eja_response:serialize(<<"articles">>, make_data(), #{}),
  ?assertEqual(
    ok,
    eja:validate_payload(Response)
  ),
  ?assertEqual(
    {error, bad_request},
    eja:validate_payload(#{})
  ),
  ?assertEqual(
    {error, bad_request},
    eja:validate_payload([])
  ),
  ?assertEqual(
    {error, bad_request},
    eja:validate_payload(make_data())
  ),
  ?assertEqual(
    {error, bad_request},
    eja:validate_payload(
      #{<<"data">> => [#{<<"foo">> => <<"bar">>}]}
    )
  ),

  ok.

%% =============================================================================

make_data() ->
  make_data(2).

make_data(N) ->
  #{
    <<"articles">> => [make_articles_data(I) || I <- lists:seq(1, N)]
  }.

make_articles_data(N) ->
  BinN = integer_to_binary(N),
  #{  <<"id">> => <<"823ec82a-5e73-4013-a253-a2abf771c6db">>
    , <<"title">> => <<"Foo", (BinN)/binary>>
    , <<"body">> => <<"Lorem impsum", (BinN)/binary>>
  }.