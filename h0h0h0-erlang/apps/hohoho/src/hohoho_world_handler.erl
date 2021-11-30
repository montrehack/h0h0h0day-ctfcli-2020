-module(hohoho_world_handler).

-export([init/2]).

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2
        ]).

-export([get_json/2, post_json/2]).

%%%==============================================
%%% Exports
%%%==============================================
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, post_json}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, get_json}
     ], Req, State}.

get_json(Req, State) ->
    {<<"{ \"hello\": \"there\" }">>, Req, State}.

post_json(Req, State) ->
    case get_request(Req) of
        {error, _Reason} ->
            {false, Req, State};
        {ok, Text} ->
            WordList = wordlist(Text),
            Encoded = jsone:encode(WordList),
            UpdatedReq = cowboy_req:set_resp_body(Encoded, Req),
            {true, UpdatedReq, State}
    end.

get_request(Req) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, _} ->
            decode_json(Data);
        {more, _, _} ->
            {error, body_too_long}
    end.

decode_json(Json) ->
    case jsone:try_decode(Json) of
        {ok, #{<<"text">> := Text}, _} when is_binary(Text) ->
            {ok, Text};
        {ok, _Body, _} ->
            {error, invalid_json};
        Error={error, _} ->
            Error
    end.

wordlist(Text) ->
    List = binary:split(Text, <<" ">>, [global, trim_all]),
    wordlist(List, []).

wordlist([], Acc) ->
    Acc;
wordlist([Word | Words], Acc) ->
    case lists:filter(fun(ChristmasWord) ->
                              Word =:= ChristmasWord end, christmas_list()) of
        [] ->
            wordlist(Words, Acc);
        _ ->
            wordlist(Words, [Word | Acc])
    end.

christmas_list() ->
    [<<"christmas">>,
     <<"santa">>,
     <<"claus">>,
     <<"tree">>,
     <<"25">>,
     <<"december">>,
     <<"elf">>,
     <<"gift">>,
     <<"gifts">>].

