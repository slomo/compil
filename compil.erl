%% -----------------------------------------------------------------------------
%% @author Yves Müller <myves.addAnAt.zedat.fu-berlin.de>
%% @copyright Simplified BSD License by Yves Müller, 2012
%% @title Compil.erl
%% @doc A simple compiler for a lispy language
%% @version 0.0.1
%% -----------------------------------------------------------------------------

-module(compil).
-export([do/1]).

%% @doc entrypoint for compilation
do(String) ->
    {ok, _Pid} = compil_table:start_link(),
    Trees = lexAndParse(String),

    addGlobalEnv(),

    EnvCodes = lists:map(fun(Tree) ->
                split_env(Tree, global) end, Trees),

%    NestingFreeCode = clear_names(EnvCode),
    compil_table:stop(),
    EnvCodes.



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
	  env :: reference(),
	  name :: string(),
      buildin :: boolean()
	 }).

-record(nameDef, {
        oldName, name, type }).

-record(lambda, {
	  ref  :: reference(),
      prefix,
      type,
	  definitions,
	  child
	 }).

-record(call,{
	  function,
	  arguments
	 }).



split_env([ 'lambda' | [Definitions | Child]], OldEnv) ->
    Ref = make_ref(),
    Prefix = compil_table:newEnv(Ref, OldEnv),
    ScannedDef = lists:map(
        fun (Def) ->
                NewName= Prefix ++ atom_to_list(Def),
                compil_table:addMemberEnv(Ref,{Def, NewName}),
                #nameDef{ oldName = Def, name = NewName }
        end, Definitions),
    ScannedChild = split_env(Child, Ref),
    #lambda{ ref = Ref, prefix = Prefix, child = ScannedChild, definitions = ScannedDef };

split_env([ Function | Arguments], Env) ->
    FunctionName = split_env(Function, Env),
    ScannedArguments = lists:map(fun (Thing) ->  split_env(Thing,Env) end, Arguments),
    #call{ function = FunctionName, arguments = ScannedArguments};

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


codegen(Expr) ->
    case Expr of
	#primOp{op=Op, type=Type, operands={A,B}} ->
	    {VarA,CodeA} = codegen(A),
	    {VarB,CodeB} = codegen(B),
	    MyVar = compil_table:newTemp(),
	    MyCode = MyVar ++ " = " ++ decode(Op) ++ " " ++ decodeType(Type) ++ " " ++ VarA ++ ", " ++ VarB,
	    {MyVar, CodeA ++ CodeB ++ [MyCode]};
	#literal{type=int, value=Val} ->
	    MyCode = [],
	    MyVar = integer_to_list(Val),
	    {MyVar, [MyCode]};
        #entry{type=Type, form=Form} ->
            {Var,Code} = codegen(Form),
            TypeStr = decodeType(Type),
            MyCode = [ "define " ++ TypeStr ++ " @main () {" ]  ++ Code ++ [ "ret " ++ TypeStr ++ " " ++ Var ++ "\n}"],
            {"main", MyCode}
    end.


% Operators
decode('+') -> "add";
decode('/') -> "div";
decode('*') -> "mul";
decode('-') -> "sub".
% Types
decodeType(int) -> "i32". 
