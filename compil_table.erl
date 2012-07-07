-module(compil_table).

-behaviour(gen_server).

-export([start_link/0,stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% -- API -----------

-export([newTemp/0]).

newTemp() ->
    gen_server:call(?MODULE,newTemp).


% -- Internals -----

-record(state,{counter,rename}).

-define(RENAME_TABLE,compil_rename_table).

start_link() ->
    gen_server:start({local, ?MODULE} ,?MODULE ,[] ,[]).

stop() ->
    gen_server:cast(?MODULE, stop).

init(_Args)->
    Table = ets:new(?RENAME_TABLE, [set,protected,named_table]),
    {ok,#state{rename=Table,counter=0}}.

handle_call(Msg, _From, State = #state{counter=C, rename=_RTable}) ->
    case Msg of
        newTemp ->
            {reply, "%tmp" ++ integer_to_list(C), State#state{counter=C+1}};
        stop ->
            {stop, normal, State}
    end.

handle_cast(Msg, State) ->
    io:printf("Unexpected cast ~f",[Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:printf("Unexpected info ~f",[Info]),
    {noreply, State}.

code_change(_OldVs, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
    
