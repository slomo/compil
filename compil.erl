%% -----------------------------------------------------------------------------
%% @author Yves Müller <myves.addAnAt.zedat.fu-berlin.de>
%% @copyright Simplified BSD License by Yves Müller, 2012
%% @title Compil.erl
%% @doc A simple compiler for a lispy language
%% @version 0.0.1
%% -----------------------------------------------------------------------------

-module(compil).
-export([do/1, do/2, precompile/1]).


%% @doc entrypoint for compilation
do(String) ->
    {ok, _} = compil_table:start_link(),
    {ok, _} = compil_emitter:start_link(),
    Pid = self(),
    spawn(fun () ->  ?MODULE:do(String, Pid) end),
    receive
        done ->
            ok;
        Value ->
            io:format("Got value ~p",Value)
    after 2000 ->
            io:format("timeout")
    end,
    compil_table:stop(),
    compil_emitter:stop().



do(String, Pid) ->
    Trees = lexAndParse(String),

    PrecompiledTrees = lists:map(fun precompile/1, Trees),

    addGlobalEnv(),

    EnvCodes = lists:map(fun(Tree) ->
                split_env(Tree, global) end, PrecompiledTrees),


    lists:map(fun (Code) -> scan_free(Code, global) end, EnvCodes),

    ClosedCodes = lists:map(fun close_var/1, EnvCodes),

    LLVM = lists:map(fun codegen/1, ClosedCodes),

    compil_emitter:function(maingen(LLVM) ++ "\n"),
    compil_emitter:to_stdout(),
    Pid ! done.



lexAndParse(String) ->
    {ok, Tokens, _Lines} = lfe_scan:string(String),
    parseAllForms(Tokens).

parseAllForms(Tokens) ->
    case lfe_parse:sexpr(Tokens) of
        {ok, _Count, Tree, []} ->
            [Tree];
        {ok, _Count, Tree, Cont} ->
            [ Tree | parseAllForms(Cont) ]
    end.





addGlobalEnv() ->
    compil_table:newEnv(global, undefined).


%    Phase2 = entryPoint(phase2(Tree)),
%    io:write(Phase2),
%    io:fwrite("~n---------~n"),
%    {_type,Typed} = type(Phase2),
%    io:write(Typed),
%    io:fwrite("~n---------~n"),
%    {_var,PreCode} = codegen(Typed),
%    io:write(PreCode),
%    io:fwrite("~n---------~n"),
%    io:fwrite(lists:foldr(
%                fun(A,B) ->
%                        case A of
%                            "" -> B;
%                            A -> A ++ " \n" ++ B
%                                     end
%                end,"", PreCode)).

-define(PRIMOPS,['+','-','*','/']).
-define(BUILDIN,?PRIMOPS).


%% TODO
% * list implementation
% * quoat
% * heap objects for passable clojures ( or maybe not)
%   * lambda lifting is needed
%   * analysis of free vars
% * introduce structure ( supervisor, writer etc ... )
% * reactivate type concluder
% * generalize everything prim op handling


-record(primOp,{
          op :: #primOp{},
          type :: systemType(),
          operands :: {form(), form()}
         }).
-record(literal,{
          type :: systemType(),
          value :: any()
         }).
-record(entry,{
          type :: systemType(),
          form :: primOp | record
         }).

-record(letForm,{
         type :: systemType(),
         bindings :: [{atom(), string(), form()}],
         form :: form()
        }).

-type primOp() :: '+' | '-' | '*' | '/'.
-type systemType() :: int | float | bool.
-type form() :: #primOp{} | #literal{}.

-record(name,{
	  name :: string(),
	  env :: reference(),
      buildin :: boolean()
	 }).

-record(nameDef, {
        oldName, name, type }).

-record(lambda, {
	  ref  :: reference(),
      label :: string(),
      type,
	  definitions,
      free_vars, % { var name, new boundname }
	  child
	 }).

-record(call,{
	  target,
	  arguments,
      kind
	 }).


%% precompiler 


precompile([ '+' | [ Arg1 | Args ]]) ->
    lists:foldl(fun(X,Y) ->  [ '+' , Y , precompile(X)] end, precompile(Arg1), Args);
precompile([ 'let' , Definitions, Child]) ->
    { FormalArguments, Parameters } = lists:unzip(lists:map( fun erlang:list_to_tuple/1, Definitions)),
    [
        [   lambda ,
            lists:map(fun precompile/1, FormalArguments),
            precompile(Child)
        ]
        | lists:map(fun precompile/1, Parameters) ];
precompile(List) when is_list(List) ->
    lists:map(fun  precompile/1,List);
precompile(Other) ->
    Other.


%% renaming of functions

split_env([ 'lambda', Definitions, Child], OldEnv) ->
    Ref = make_ref(),
    Prefix = compil_table:newEnv(Ref, OldEnv),
    ScannedDef = lists:map(
        fun (Def) ->
                NewName= Prefix ++ "_" ++ atom_to_list(Def),
                compil_table:addMemberEnv(Ref,{Def, NewName}),
                #nameDef{ oldName = Def, name = NewName }
        end, Definitions),
    ScannedChild = split_env(Child, Ref),
    #lambda{ ref = Ref, label = Prefix, child = ScannedChild, definitions = ScannedDef };

split_env([ Function | Arguments], Env) ->
    Target = split_env(Function, Env),
    ScannedArguments = lists:map(fun (Thing) ->  split_env(Thing,Env) end, Arguments),
    BasicCall = #call{ target = Target, arguments = ScannedArguments};

split_env(Atom, Env) when is_atom(Atom) ->
    case lists:member(Atom,?BUILDIN) of
        true ->
            #name{name=Atom, buildin=true};
        _   ->
            {NameEnv, NewName} = lookup_name(Env, Atom),
            #name{name = NewName, env = NameEnv, buildin=false }
    end;

%split_env(StringLit) when is_string(StringLit) ->
%    #literal{ type=string, value=StringLit };
split_env(IntLit, _Env) when is_integer(IntLit) ->
    #literal{ type=int, value=IntLit };
split_env(BoolLit, _Env) when is_boolean(BoolLit) ->
    #literal{ type=bool, value=BoolLit}.


lookup_name(Env, Name) ->
    Members  = compil_table:getMembersEnv(Env),
    case proplists:lookup(Name, Members) of
        {Name,NewName} ->
            {Env, NewName};
        none when Env /= global ->
            lookup_name(compil_table:getParentEnv(Env), Name);
        _ ->
            "Error name not found"
    end.


%% free variables analysis

scan_free(#lambda{ child=Child, ref=Ref }, _ParentRef) ->
    scan_free(Child, Ref);
scan_free(#name{ buildin = true }, Ref) ->
    done;
scan_free(#name{ name = Name}, Ref) ->
    add_free_var(Ref, Name) ;
scan_free(#call{ target = Target, arguments = Arguments}, Ref) ->
    scan_free(Target,Ref),
    lists:map(fun (Arg) -> scan_free(Arg,Ref) end, Arguments),
    done;
scan_free(_Other, _Env) ->
    done.


add_free_var(Ref,Name) ->
    { _ , Members }  = lists:unzip(compil_table:getMembersEnv(Ref)),
    Free = compil_table:getFreeEnv(Ref),
    case {lists:member(Name, Members), lists:member(Name, Free)} of 
        {false, _ } when Ref =:= global ->
            io:format("Error name not found: " ++ Name);
        {false, false} ->
            compil_table:addFreeEnv(Ref,Name),
            ParentRef = compil_table:getParentEnv(Ref),
            add_free_var(ParentRef, Name);
        {_, _} ->
            done
    end.
    





%% close all vars

close_var(Call = #call{ target=(Target = #lambda{ ref = Ref, definitions = Defs, 
                child=Child }) , arguments = Args})  ->
    Free = compil_table:getFreeEnv(Ref),
    FreeDefs = lists:map(fun(Name) -> #nameDef{name=Name} end, Free),
    NewChild = close_var(Child),
    NewTarget = Target#lambda{definitions = Defs ++ FreeDefs, child=NewChild},
    NewArguments = lists:map( fun close_var/1, Args),
    FreeArguments = lists:map(fun(Var) -> #name{name=Var} end, Free), 
    Call#call{ target=NewTarget, arguments=NewArguments ++ FreeArguments };
close_var(Call = #call{ arguments = Args}) ->
    Call#call{ arguments = lists:map( fun close_var/1, Args)};
close_var(Other) ->
    Other.

%phase2([ 'let' | [Bindings | Form]]) ->
%    Definitons = lists:map(fun ([ Name, Form ]) when is_atom(Def) ->
%                NewName = compil_table:newUnique(Def),
%                {{Name, NewName}, Form} end, Bindings),




phase2([Head | Forms]) ->
    case lists:member(Head,?PRIMOPS) of
        true ->
            foldPrimOp(Head, Forms);
        false ->
            io:write({error,{unknowExpressionType,Head}})
    end;
phase2(Lit) when is_integer(Lit) ->
    #literal{type=int,value=Lit}.



foldPrimOp(Op,[Arg1|Args]) ->
    lists:foldl(fun(X,Y) -> #primOp{op=Op,operands={Y,phase2(X)}} end, phase2(Arg1), Args).


entryPoint(Form) ->
    #entry{form=Form}.


type(Expr) ->
    case Expr of
        #primOp{operands={A,B}} ->
            {TypeA,NewA} = type(A),
            {TypeB,NewB} = type(B),
            MyType = maxType(TypeA,TypeB),
            {MyType,Expr#primOp{type=MyType,operands={NewA,NewB}}};
        #literal{type=Type} ->
            {Type,Expr};
        #entry{form=Form} ->
            {MyType,NewForm} = type(Form),
            {MyType,Expr#entry{form=NewForm,type=MyType}}
    end.

maxType(int,int) ->
    int.

% code gen for the known  type


codegen(#lambda{label = Label, definitions=Defs, child = Child}) ->
    Type = "i32",
    Header1 = "define " ++ Type ++ " @" ++ Label ++ " ( ",
    Arguments = lists:map(
        fun
            (#nameDef{ name=Name }) -> "i32 %" ++  Name
        end,Defs),
    ArgumentStr = string:join(Arguments,", "),
    Header = Header1 ++ ArgumentStr ++ ") \n {",
    {ChildVar, ChildCode} = codegen(Child),
    Footer = "\nret " ++ decodeType(int) ++ " " ++ ChildVar ++ " }\n",
    compil_emitter:function(Header ++ ChildCode ++ Footer ++ "\n"),
    {"@" ++ Label,""};

codegen(#call{ target = Target,  arguments = Args }) ->
    CodeArgs = lists:map(fun(Arg) ->  codegen(Arg) end, Args),
    {Vars, ArgCodes}  = lists:unzip(CodeArgs),
    ArgCode =  string:join(lists:filter( fun (A) -> [] /= A end,ArgCodes), "\n"),
    case Target of
        #name{ name='+' } ->
            MyVar = compil_table:newTemp(),
            [VarA, VarB] = Vars,
            MyCode = ArgCode ++  "\n" ++ MyVar ++ " = " ++ decode('+') ++ " " ++ decodeType(int)
                        ++ " " ++ VarA ++ ", " ++ VarB,
            {MyVar,MyCode};
        #lambda{ } = Lambda ->
            {LambdaVar, _ } = codegen(Lambda),
            MyVar = compil_table:newTemp(),
            Vars2 = lists:map( fun(A) -> "i32 " ++ A end, Vars),
            VarString = string:join(Vars2,", "),
            MyCode = ArgCode ++ "\n" ++  MyVar ++ " = call i32 "
                ++ LambdaVar ++ "(" ++ VarString ++ ")",
            {MyVar, MyCode}

    end;
codegen(#name{ name=Var}) ->
    { "%" ++ Var, []};

codegen(Expr) ->
    case Expr of
	#primOp{op=Op, type=Type, operands={A,B}} ->
	    {VarA,CodeA} = codegen(A),
	    {VarB,CodeB} = codegen(B),
	    MyVar = compil_table:newTemp(),
	    MyCode = MyVar ++ " = " ++ decode(Op) ++ " " ++ decodeType(Type) ++ " " ++ VarA ++ ", " ++ VarB,
	    {MyVar, CodeA ++ CodeB ++ [MyCode]};
	#literal{type=int, value=Val} ->
	    MyCode = "",
	    MyVar = integer_to_list(Val),
	    {MyVar, MyCode}
end.


maingen(Exprs) ->
    [ Final | _Others ] = lists:reverse(Exprs),
    {_, Code} = lists:unzip(Exprs),
    CodeStr = string:join(Code,"\n"),
    {Var, _} = Final,
    TypeStr = decodeType(int),
    MyCode = "define " ++ TypeStr ++ " @main () {"  ++ CodeStr  ++ "\nret " ++ TypeStr ++ " " ++ Var ++ "\n}".



% Operators
decode('+') -> "add";
decode('/') -> "div";
decode('*') -> "mul";
decode('-') -> "sub".
% Types
decodeType(int) -> "i32".
