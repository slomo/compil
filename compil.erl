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

    TypedCodes = lists:map(fun type/1, ClosedCodes),
    io:write(TypedCodes),
    io:format("\n------\n"),


    LLVM = lists:map(fun codegen/1, TypedCodes),

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
% * heap objects for passable clojures
% * reactivate type concluder
% * generalize everything prim op handling

-record(literal,{
          type :: systemType(),
          value :: any()
         }).

-type systemType() :: int | float | bool.

-record(name,{
	  name :: string(),
	  env :: reference(),
      type,
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
      kind,
      type
	 }).

-record(closure,{
    lambda,
    captures = []
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
    #call{ target = Target, arguments = ScannedArguments};

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
scan_free(#name{ buildin = true }, _Ref) ->
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
close_var(Lambda = #lambda{ ref=Ref }) ->
    Free = compil_table:getFreeEnv(Ref),
    FreeArguments = lists:map(fun(Var) -> #name{name=Var} end, Free),
    #closure{ lambda=Lambda, captures = FreeArguments };
close_var(Other) ->
    Other.



% types

%% TODO:
%
% * function types


-record(typeFun,{arguments=[],return}).


type(#closure{lambda=Lambda, captures=Captures} = Closure) ->
    TypedLambda = type(Lambda),
    TypedCaptures = lists:map(
        fun
            (#nameDef{ name=Name}=Def) ->
                Type = compil_table:getType(Name),
                Def#name{type=Type}
        end, Captures),
    Closure#closure{ lambda=TypedLambda, captures=TypedCaptures };

type(#lambda{definitions = Definitions, child = Child} = Lambda) ->
    TypedChild = type(Child),
    {ArgTypes,TypedDefinitions} = lists:unzip(lists:map(
        fun
            (#nameDef{ name=Name}=Def) ->
                Type = compil_table:getType(Name),
                {Type, Def#nameDef{type=Type}}
        end, Definitions)),
    MyType = #typeFun{return=extract_type(TypedChild), arguments=ArgTypes},
    Lambda#lambda{definitions=TypedDefinitions, child=TypedChild, type=MyType};

type(#call{ target=Target, arguments=Arguments} = Call ) ->
    TypedTarget = type(Target),
    #typeFun{ return=ReturnType, arguments=ExpectedArgTypes} = extract_type(TypedTarget),
    length(Arguments) == length(ExpectedArgTypes) orelse throw(to_few_arguments),
    TypedArguments =lists:map(
            fun
                ({Argument, ExpectedType}) ->
                    ensure_type(Argument, ExpectedType)
            end,lists:zip(Arguments, ExpectedArgTypes)),
    Call#call{ target = TypedTarget, arguments=TypedArguments, type=ReturnType};
type(#name{name=Name, buildin=true} = Expr) ->
    Expr#name{type=buildin_type(Name)};
type(#name{name=Name} = Expr) ->
    Type = compil_table:getType(Name),
    Expr#name{type=Type};
type(#literal{} = Lit) ->
    Lit.



ensure_type(Expr = #name{ name=Name },Type) ->
    case compil_table:getType(Name) of
        Type ->
            ok;
        undefined ->
            compil_table:setType(Name, Type)
    end,
    type(Expr);
ensure_type(Expr, Type) ->
    TypedExpr = type(Expr),
    Type = extract_type(TypedExpr),
    TypedExpr.

extract_type(#call{ type = Type}) -> Type;
extract_type(#name{type = Type}) -> Type;
extract_type(#literal{type = Type}) -> Type;
extract_type(#lambda{type = Type}) -> Type;
extract_type(#closure{ lambda=#lambda{type = Type}}) -> Type.



% @doc: show which type may be mapped to int
%conversions(int,float) ->
%    float.

buildin_type('+') ->
    #typeFun{arguments=[int, int], return=int}.



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
            MyCode = ArgCode ++  "\n" ++ MyVar ++ " = " ++
                decode('+') ++ " " ++ decodeType(int)
                        ++ " " ++ VarA ++ ", " ++ VarB,
            {MyVar,MyCode};
        #lambda{ } = Lambda ->
            {LambdaVar, _ } = codegen(Lambda),
            MyVar = compil_table:newTemp(),
            Vars2 = lists:map( fun(A) -> "i32 " ++ A end, Vars),
            VarString = string:join(Vars2,", "),
            MyCode = ArgCode ++ "\n" ++  MyVar ++ " = call i32 "
                ++ LambdaVar ++ "(" ++ VarString ++ ")",
            {MyVar, MyCode};
        #name{ name=Name, buildin=false } ->
            MyVar = compil_table:newTemp(),
            Vars2 = lists:map( fun(A) -> "i32 " ++ A end, Vars),
            VarString = string:join(Vars2,", "),
            MyCode = ArgCode ++ "\n" ++  MyVar ++ " = call i32 %"
                ++ Name ++ "(" ++ VarString ++ ")",
            {MyVar, MyCode}
    end;

codegen(#name{ name=Var}) ->
    { "%" ++ Var, []};

codegen(#closure{ lambda = Lambda, captures= _Captures }) ->
    { _FunctionName, _ } = codegen(Lambda);

codegen(#literal{type=int, value=Val}) ->
    MyCode = "",
    MyVar = integer_to_list(Val),
    {MyVar, MyCode}.


maingen(Exprs) ->
    [ Final | _Others ] = lists:reverse(Exprs),
    {_, Code} = lists:unzip(Exprs),
    CodeStr = string:join(Code,"\n"),
    {Var, _} = Final,
    TypeStr = decodeType(int),
    "define " ++ TypeStr ++ " @main () {"
        ++ CodeStr  ++ "\nret " ++ TypeStr ++ " " ++ Var ++ "\n}".



% Operators
decode('+') -> "add";
decode('/') -> "div";
decode('*') -> "mul";
decode('-') -> "sub".
% Types
decodeType(int) -> "i32".
