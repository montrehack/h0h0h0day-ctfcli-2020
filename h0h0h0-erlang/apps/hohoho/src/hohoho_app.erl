%%%-------------------------------------------------------------------
%% @doc hohoho public API
%% @end
%%%-------------------------------------------------------------------

-module(hohoho_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ssh:daemon(any, 8989, [{system_dir, "ssh_keys"}, {user_passwords, [{"patate", "patate"}]}]),
    hohoho_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
