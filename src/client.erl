-module(client).

-export([start_link/1]).

-export([say/2, badsay/2]).

-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).


say(Name, Msg) ->
    call(Name, {say, Msg}).

badsay(Name, Msg) ->
    call(Name, {bad, Msg}).


start_link(Name) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Name, []).


init(Name) ->
    ok = server:register_client(Name),
    {ok, Name}.

handle_call({say, Msg}, _From, Name) ->
    io:format("~p say: ~p", [Name, Msg]),
    {reply, ok, Name}.

handle_cast(_Msg, Name) ->
    {noreply, Name}.

handle_info(_Msg, Name) ->
    {noreply, Name}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _Name) ->
    ok.


call(Name, Msg) ->
    {ok, Pid} = server:get_client(Name),
    gen_server:call(Pid, Msg).
