%%%-------------------------------------------------------------------
%% @doc hohoho top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hohoho_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 10},
    ChildSpecs = [#{id => hohoho_secret,
                    start => {hohoho_secret, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [hohoho_secret]},
                  #{id => hohoho_web,
                    start => {hohoho_web, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [hohoho_web]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
