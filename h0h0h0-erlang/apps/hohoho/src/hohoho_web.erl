-module(hohoho_web).

-behavior(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    RanchOptions = [{port, 8080}, {ip, {0,0,0,0}}],
    Routes = [
              {"/api/words", hohoho_world_handler, []},
              {"/static/[...]", cowboy_static, {priv_dir, hohoho, "webapp/build/static"}},
              {"/[...]", cowboy_static, {priv_file, hohoho, "webapp/build/index.html"}}
             ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_clear(?MODULE,
                                 RanchOptions,
                                 #{env => #{dispatch => Dispatch},
                                   middlewares => [cowboy_router, cowboy_handler]}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_, _) ->
    cowboy:stop_listener(?MODULE).


