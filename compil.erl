%% -----------------------------------------------------------------------------
%% @author Yves Müller <myves.addAnAt.zedat.fu-berlin.de>
%% @copyright Simplified BSD License by Yves Müller, 2012
%% @title Compil.erl
%% @doc A simple compiler for a lispy language
%% @version 0.0.1
%% -----------------------------------------------------------------------------

-module(compil).
-export([do/1, do/2, fromFileToFile/2]).


runAsProcess(String) ->
    Pid = self(),
    spawn(fun () ->  ?MODULE:do(String, Pid) end),
    receive
        done ->
            ok;
        Value ->
            io:format("Got value ~p",Value)
    after 2000 ->
            io:format("timeout"),
            timeout
    end.

%% @doc entrypoint for compilation
do(String) ->
    {ok, _} = compil_table:start_link(),
    {ok, _} = compil_emitter:start_link(),
    runAsProcess(String),
    compil_emitter:to_stdout(),
    compil_table:stop(),
    compil_emitter:stop().


fromFileToFile(SourceFileName, TargetFileName) ->
    {ok, String} = file:read_file(SourceFileName),
    {ok, _} = compil_table:start_link(),
    {ok, _} = compil_emitter:start_link(),
    case runAsProcess(binary_to_list(String)) of
        ok ->
            compil_emitter:to_file(TargetFileName),
            compil_table:stop(),
            compil_emitter:stop();
        timeout ->
            compil_table:stop(),
            compil_emitter:stop(),
            throw(compil_error)
    end.



do(String, Pid) ->
    Trees = lexAndParse(String),

    PrecompiledTrees = lists:map(fun precompile/1, Trees),

    addGlobalEnv(),

    EnvCodes = lists:map(fun(Tree) ->
                split_env(Tree, global) end, PrecompiledTrees),


    lists:map(fun (Code) -> scan_free(Code, global) end, EnvCodes),

    ClosedCodes = lists:map(fun close_var/1, EnvCodes),

    TypedCodes = lists:map(fun type/1, ClosedCodes),

    LLVM = lists:map(fun codegen/1, TypedCodes),

    compil_emitter:function(maingen(LLVM) ++ "\n"),
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


%% TODO:
% * list implementation
% * quote
% * heap objects for passable clojures
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
    type,
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
            throw({unkown_name, Name});
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
close_var(Call = #call{ arguments = Args, target=Target}) ->
    % TODO: close also the target? Enabled for testing purpose
    Call#call{ target =  close_var(Target), arguments = lists:map( fun close_var/1, Args)};

close_var(Lambda = #lambda{ ref=Ref, definitions=Defs, child=Child}) ->
    NewChild = close_var(Child),
    Free = compil_table:getFreeEnv(Ref),
    FreeDefs = lists:map(fun(Name) -> #nameDef{name=Name} end, Free),
    NewLambda = Lambda#lambda{definitions = Defs ++ FreeDefs, child=NewChild},
    FreeArguments = lists:map(fun(Var) -> #name{name=Var} end, Free),
    #closure{ lambda=NewLambda, captures = FreeArguments };

close_var(Other) ->
    Other.



% types

-record(typeFun,{arguments=[],return}).
-record(typeCl,{lambda,captures=[]}).

type(#closure{lambda=Lambda, captures=Captures} = Closure) ->
    TypedLambda = type(Lambda),
    {TypedCaptures, CaptureTypes}  = lists:unzip(lists:map(
        fun
            (#name{ name=Name}=Def) ->
                Type = compil_table:getType(Name),
                {Def#name{type=Type},Type}
        end, Captures)),
    Closure#closure{ type=#typeCl{ lambda = TypedLambda#lambda.type, captures=CaptureTypes }, lambda=TypedLambda, captures=TypedCaptures };

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
    case extract_type(TypedTarget) of
        #typeFun{ return=ReturnType, arguments=ExpectedArgTypes} ->
            TypedArguments = type_check_args(Arguments, ExpectedArgTypes);
        #typeCl{ captures=CaptureTypes, lambda = #typeFun{ return=ReturnType, arguments=ExpectedArgTypes} } ->
            TypedArguments = type_check_args(Arguments, lists:subtract(ExpectedArgTypes, CaptureTypes))
    end,
    Call#call{ target = TypedTarget, arguments=TypedArguments, type=ReturnType};

type(#name{name=Name, buildin=true} = Expr) ->
    Expr#name{type=buildin_type(Name)};
type(#name{name=Name} = Expr) ->
    Type = compil_table:getType(Name),
    Expr#name{type=Type};
type(#literal{} = Lit) ->
    Lit.

% returns arguments with type information
type_check_args(Got, Expected) ->
    length(Got) == length(Expected) orelse throw(invalid_number_of_arguements),
    lists:map(
        fun
            ({Argument, ExpectedType}) ->
                ensure_type(Argument, ExpectedType)
        end,lists:zip(Got, Expected)).


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
extract_type(#closure{ type= Type}) -> Type.



% @doc: show which type may be mapped to int
%conversions(int,float) ->
%    float.

buildin_type('+') ->
    #typeFun{arguments=[int, int], return=int}.


-define(NOCODE,[]).
-define(LIN(Tokens), string:join(Tokens," ")).
-define(FUNCTION(Lines), hd(Lines) ++ "{\n\t" ++ string:join(tl(Lines),"\n\t") ++ "\n}\n").

% code gen for the known  type
codegen(#lambda{label = Label, definitions=Defs, child = Child}) ->
    RetType = edt(Child),
    Arguments = lists:map(
        fun (#nameDef{ name=Name, type=Type }) -> lists:concat([decode_type(Type), " %", Name]) end,
        Defs),
    {ChildVar, ChildLines} = codegen(Child),
    HeaderLine = ?LIN(["define", RetType, "@" ++ Label, "(", string:join(Arguments,", "), ")"]),
    ReturnLine = ?LIN(["ret", RetType, ChildVar]),
    Code = ?FUNCTION([ HeaderLine | lists:reverse(ChildLines) ] ++ [ ReturnLine ]),
    compil_emitter:function(Code),
    {"@" ++ Label,""};

codegen(#call{ type=RetType, target = Target,  arguments = Args }) ->

    CodeArgs = lists:map(fun(Arg) ->  codegen(Arg) end, Args),
    {Vars, ArgCodes}  = lists:unzip(CodeArgs),
    ArgCodeLines =  lists:concat(ArgCodes),
    case Target of
        #name{ name='+' } ->
            MyVar = compil_table:newTemp(),
            [VarA, VarB] = Vars,
            MyOp = ?LIN([MyVar,"=", decode('+'), decode_type(RetType), VarA,",",VarB]),
            {MyVar, [ MyOp | ArgCodeLines ]};
        #lambda{ type=#typeFun {arguments=ArgTypes} } = Lambda ->
            {LambdaVar, _ } = codegen(Lambda),
            MyVar = compil_table:newTemp(),
            ArgsStr = lists:map( fun({Var, Type}) -> decode_type(Type) ++ " " ++  Var end, lists:zip(Vars, ArgTypes)),
            VarString = string:join(ArgsStr,", "),
            MyCall = ?LIN([MyVar,"=","call",decode_type(RetType), LambdaVar, "(", VarString ,")"]),
            {MyVar, [ MyCall | ArgCodeLines ]};
        Other -> % callee must be a closure
            ClType = #typeCl{ } = extract_type(Other),
            % FIXME: dropping importand code (if the other was a function eg)
            { StructPointer, PrevCodes} = codegen(Other),

            FunctionPointerPointer = compil_table:newTemp(),
            FunctionPointer = compil_table:newTemp(),
            BindPointer = compil_table:newTemp(),
            Binding = compil_table:newTemp(),

            ComputeFPPCode = ?LIN([FunctionPointerPointer,"=","getelementptr", decode_type(ClType),StructPointer ++ ",","i32 0,","i32 0"]),
            ComputeBPCode = ?LIN([BindPointer,"=","getelementptr",decode_type(ClType),StructPointer ++ ",","i32 0,","i32 2"]),

            LoadFPCode = ?LIN([FunctionPointer,"=","load","i32(i32,i32)**",FunctionPointerPointer]),
            LoadBCode = ?LIN([Binding,"=","load","i32*",BindPointer]),

            % FIXME: dirty hack [int]
            ArgsStr = lists:map( fun({Var, Type}) -> decode_type(Type) ++ " " ++  Var end, lists:zip(Vars, [int])),
            VarString = string:join(ArgsStr,", "),
            MyVar = compil_table:newTemp(),
            CallCode = ?LIN([MyVar,"=","call",decode_type(RetType),FunctionPointer,"(",VarString,", i32",Binding,")"]),

            { MyVar, lists:reverse([ComputeFPPCode,ComputeBPCode,LoadFPCode,LoadBCode,CallCode]) ++ PrevCodes }

    end;

codegen(#name{ name=Var}) ->
    { "%" ++ Var, ?NOCODE};


% TODO:
% * introduce clojure type
% * parameterize for different sizes of bindings
% * write cll code for clojure

codegen(#closure{ lambda = Lambda = #lambda { type=Type },type = ClType,  captures=  [ #name{name=CaptureName, type=CaptureType }]}) ->
    { FunctionName, _ } = codegen(Lambda),
    % Typedef := { Pointer to Function, Number of Bindings, Binding }
    compil_emitter:function(?LIN(["%closure","=","type","{","i32(i32,i32)*,", "i32,","i32","}"])),
    StructPointer = compil_table:newTemp(),
    FunctionPointer = compil_table:newTemp(),

    ClojureStructType = decode_type(ClType),
    AllocCode = ?LIN([StructPointer,"=","alloca","%closure"]),
    GetFPCode = ?LIN([FunctionPointer,"=","getelementptr",ClojureStructType,StructPointer++ ",","i32 0,","i32 0"]),
    StoFPCode = ?LIN(["store","i32(i32,i32)*",FunctionName ++ "," ,"i32(i32,i32)**",FunctionPointer]),

    BindPointer = compil_table:newTemp(),
    GetBPCode = ?LIN([BindPointer,"=","getelementptr",ClojureStructType,StructPointer++",","i32 0,","i32 2"]),
    StoBPCode = ?LIN(["store","i32","%"++CaptureName ++ ",","i32*",BindPointer]),

    {StructPointer, lists:reverse([AllocCode, GetFPCode, StoFPCode, GetBPCode, StoBPCode])};




codegen(#literal{type=int, value=Val}) ->
    MyVar = integer_to_list(Val),
    {MyVar, ?NOCODE}.


maingen(Exprs) ->
    Final = lists:last(Exprs),
    {_, Code} = lists:unzip(Exprs),
    Lines = lists:concat(lists:map(fun lists:reverse/1, Code)),
    {FinalVar, FinalCode} = Final,
    RetType = decode_type(int), % defined to be int, other make no sense

    ?FUNCTION(
            [ ?LIN([ "define", RetType, "@main", "()"]) | Lines ] ++
            [ ?LIN(["ret", RetType, FinalVar]) ]
    ).


edt(Expr) -> decode_type(extract_type(Expr)).


% Operators
decode('+') -> "add";
decode('/') -> "div";
decode('*') -> "mul";
decode('-') -> "sub".


% Types
decode_type(#typeFun{return=Return,arguments=Args}) ->
    decode_type(Return) ++ "(" ++ string:join(lists:map(fun decode_type/1, Args)," ,") ++ ")*";
decode_type(#typeCl{ }) -> "%closure*";
decode_type(int) -> "i32".
