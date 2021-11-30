-module(hohoho_secret).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, #{flag => "FLAG-70328b0cdf44952ca1feb9f83b32fa88"}}.

handle_call(_Request, _From, State) ->
    {reply, "Debug me plz", State}.


handle_cast(_Request, State) ->
    {noreply, State}.



