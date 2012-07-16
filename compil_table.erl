%% -----------------------------------------------------------------------------
%% @author Yves Müller <myves.addAnAt.zedat.fu-berlin.de>
%% @copyright Simplified BSD License by Yves Müller, 2012
%% @title Compil.erl
%% @doc A table server, for supporting the Compil compiler
%% @version 0.0.1
%% -----------------------------------------------------------------------------


-module(compil_table).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).


-define(RENAME_TABLE,compil_rename_table).
-define(ENV_TABLE,compil_env_table).

% -- Entries -------------

-record(envEntry,{ ref, parent}).

% -- API -----------

-export([newTemp/0, newEnv/2, getParentEnv/1]).

newTemp() ->
    gen_server:call(?MODULE, newTemp).

newUnique(Atom) ->
    gen_server:call(?MODULE, {newUnique, Atom}).


newEnv(Ref,Parent) ->
    gen_server:call(?MODULE, {store, ?ENV_TABLE, #envEntry{ref = Ref, parent = Parent}}).

getParentEnv(Ref) ->
    #envEntry{ parent = Parent } = gen_server:call(?MODULE, {get, ?ENV_TABLE, Ref}),
    Parent.


% -- Internals -----

-record(state,{counter,rename}).

start_link() ->
    gen_server:start({local, ?MODULE} ,?MODULE ,[] ,[]).
%    case whereis(?MODULE) of
%        Pid when is_pid(Pid) ->
%            {ok, Pid};
%        undefined ->
%            gen_server:start({local, ?MODULE} ,?MODULE ,[] ,[])
%    end.

stop() ->
    gen_server:cast(?MODULE, stop).

init(_Args)->
    Table = ets:new(?RENAME_TABLE, [set,protected,named_table]),
    ets:new(?ENV_TABLE, [set,protected,named_table,{keypos,2}]),
    {ok,#state{rename=Table,counter=0}}.

handle_call(Msg, _From, State = #state{counter=C, rename=RTable}) ->
    case Msg of
        newTemp ->
            {reply, "%tmp" ++ integer_to_list(C), State#state{counter=C+1}};
        {newUnique,Atom} ->
            case ets:lookup(RTable, Atom) of
                [{Atom, Counter}] ->
                    ets:insert(RTable,{Atom,Counter+1}),
                    {reply, atom_to_list(Atom) ++ integer_to_list(Counter) ++ "_", State}
	    end;
        stop ->
	        {stop, normal, State};
	{store, Table, Entry} ->
	    ets:insert(Table, Entry),
        {reply, ok, State};
    {get, Table, Key} ->
        [H|[]] = ets:lookup(Table, Key),
        H
    end.

handle_cast(Msg, State) ->
    io:printf("Unexpected cast ~f",[Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:printf("Unexpected info ~f",[Info]),
    {noreply, State}.

code_change(_OldVs, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok.