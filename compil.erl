%% -----------------------------------------------------------------------------
%% @author Yves Müller <myves*addAnAt*zedat.fu-berlin.de>
%% @copyright Simplified BSD License by Yves Müller, 2012
%% @title Compil.erl
%% @doc A simple compiler for a lispy language
%% @version 0.0.1
%% -----------------------------------------------------------------------------

-module(compil).
-export([do/1]).

%% @doc entrypoint for compilation
do(String) ->
    compil_table:start_link(),
    {ok,Tokens,_Lines} = lfe_scan:string(String),
    {ok,1,Tree,_Cont} = lfe_parse:sexpr(Tokens),
    Phase2 = entryPoint(phase2(Tree)),
    io:write(Phase2),
    io:fwrite("~n---------~n"),
    {_type,Typed} = type(Phase2),
    io:write(Typed),
    io:fwrite("~n---------~n"),
    {_var,PreCode} = codegen(Typed),
    io:write(PreCode),
    io:fwrite("~n---------~n"),
    io:fwrite(lists:foldr(
                fun(A,B) ->
                        case A of
                            "" -> B;
                            A -> A ++ " \n" ++ B
                                     end
                end,"", PreCode)).

-type primOp() :: '+' | '-' | '*' | '/'.
-type systemType() :: int | float | bool.
-type from() :: #primOp | #literal.
-define(PRIMOPS,['+','-','*','/']).



-record(primOp,{
          op :: primeOp(),
          type :: systemType()
          operands :: {#form(), #form()}
         }).
-record(literal,{
          type :: systemType(),
          value :: any()
         }).
-record(entry,{
          type :: systemType(),
          form :: primOp | record 
         }).

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
