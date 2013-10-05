
-module(client_sup).

-behaviour(supervisor).

%% API
-export([start_client/1]).
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


-define(CLIENT(Name),
        {Name, {client, start_link, [Name]}, permanent, 5000, worker,
        [Name]}).

start_client(Name) ->
    supervisor:start_child(?MODULE, [Name]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

    {ok, { {simple_one_for_one, 5, 10},
         [{client,
           {client, start_link, []},
           permanent, 5000, worker, [client]}]} }.

