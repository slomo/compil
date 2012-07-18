%% -----------------------------------------------------------------------------
%% @author Yves Müller <myves.addAnAt.zedat.fu-berlin.de>
%% @copyright Simplified BSD License by Yves Müller, 2012
%% @title compil_emitter.erl
%% @doc The code emitter for the compil compiler
%% @version 0.0.1
%% -----------------------------------------------------------------------------
-
module(compil_emitter).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% api

-export([function/1, to_file/1, to_stdout/0, to_lli/0]).

function(Code) ->
    gen_server:call(?MODULE, {function,Code}).


to_file(Filename) ->
    Content = gen_server:call(?MODULE, return_all),
    file:write(Filename,<<Content>>).

to_stdout() ->
    Content = gen_server:call(?MODULE, return_all),
    io:fwrite(Content).

to_lli() ->
    Content = gen_server:call(?MODULE, return_all),
    io:fwrite("\n------- Begin code -------\n"),
    io:fwrite(Content),
    io:fwrite("\n------- End code -------\n").


% internal

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

-record(state,{functions=[], strings=[], types=[]}).



init(_Args) ->
    {ok, #state{}}.

handle_call(return_all, _From, State = #state{ functions=Functions} ) ->
    {reply, string:join(lists:reverse(Functions),"\n\n"), State};
handle_call({function,Code}, _From, State = #state{ functions=Functions} ) ->
    {reply, ok, State#state{functions=[Code | Functions]}}.

handle_cast(stop, State) ->
	{stop, normal, State};
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
