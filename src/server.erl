-module(server).

-export([register_client/1, get_client/1]).
-export([start_link/0]).


-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).


register_client(Name) ->
    gen_server:call(?MODULE, {register, Name, self()}).

get_client(Name) ->
    gen_server:call(?MODULE, {get, Name}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    %register(Name, self()),
    {ok, dict:new()}.

handle_call({register, Name, Pid}, _From, Names) ->
    io:format("register ~p", [Name]),
    erlang:monitor(process, Pid),
    put(Pid, Name),

    {reply, ok, dict:store(Name, Pid, Names)};

handle_call({get, Name}, _From, Names) ->
    case dict:find(Name, Names) of
        {ok, Pid} ->
            {reply, {ok, Pid}, Names};
        _ ->
            {reply, notfound, Names}
    end.

handle_cast(_Msg, Name) ->
    {noreply, Name}.

handle_info({'DOWN', _, _, Pid, _}, Names) ->
    case get(Pid) of
        undefined ->
            {noreply, Names};
        Name ->
            {noreply, dict:erase(Name, Name)}
    end;

handle_info(_Msg, Name) ->
    {noreply, Name}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _Name) ->
    ok.
