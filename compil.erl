%% -----------------------------------------------------------------------------
%% @author Yves Müller <myves.addAnAt.zedat.fu-berlin.de>
%% @copyright Simplified BSD License by Yves Müller, 2012
%% @title Compil.erl
%% @doc A simple compiler for a lispy language
%% @version 0.0.1
%% -----------------------------------------------------------------------------

-module(compil).
-export([do/1, lli/1, do/2, fromFileToFile/2]).

%% TODO:
% * list implementation
% * quote
% * closure
%   * ( rather store on head, than in stackframe)

%% Problems:
% * when shall i free the allocated storage for closures

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

lli(String) ->
    {ok, _} = compil_table:start_link(),
    {ok, _} = compil_emitter:start_link(),
    runAsProcess(String),
    Ret = compil_emitter:to_lli(),
    compil_table:stop(),
    compil_emitter:stop(),
    Ret.


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


-define(BOOL_OPS,['and','or']).
-define(CMP_OPS,['=','<','>']).
-define(NUM_OPS,['+','-','*','/']).

-define(PRIMOPS,?BOOL_OPS ++ ?CMP_OPS ++ ?NUM_OPS).
-define(BUILDINS,?PRIMOPS ++ ['cond', fix]).

-record(literal,{
          type :: systemType(),
          value :: any()
         }).

-type systemType() :: int | float | bool.

-record(name,{
	  name :: string(),
	  env :: reference(),
      type,
      buildin :: boolean(),
      primitiv :: boolean()
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

split_env(IntLit, _Env) when is_integer(IntLit) ->
    #literal{ type=int, value=IntLit };
split_env(BoolLit, _Env) when is_boolean(BoolLit) ->
    #literal{ type=bool, value=BoolLit};

split_env(Atom, Env) when is_atom(Atom) ->
    case {lists:member(Atom,?BUILDINS),lists:member(Atom,?PRIMOPS)} of
        {false, false}  ->
            {NameEnv, NewName} = lookup_name(Env, Atom),
            #name{name = NewName, env = NameEnv, buildin=false };
        {IsBuildin, IsPrimitiv} ->
            #name{name=Atom, buildin=IsBuildin, primitiv=IsPrimitiv}
    end.

%split_env(StringLit) when is_string(StringLit) ->
%    #literal{ type=string, value=StringLit };


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


-record(fix,{closure,type}).

% types
-record(typeFun,{arguments=[],return}).

% This record represents a clsoure type. The name fild, is the type name
% later on choosen for the struct, representing this closure
-record(typeCl,{lambda,name,captures=[]}).

type(#closure{lambda=Lambda, captures=Captures} = Closure) ->
    TypedLambda = type(Lambda),
    {TypedCaptures, CaptureTypes}  = lists:unzip(lists:map(
        fun
            (#name{ name=Name}=Def) ->
                Type = compil_table:getType(Name),
                {Def#name{type=Type},Type}
        end, Captures)),
    Closure#closure{ type=#typeCl{
            name= "clojure_" ++ Lambda#lambda.label,
            lambda = TypedLambda#lambda.type,
            captures=CaptureTypes },
        lambda=TypedLambda, captures=TypedCaptures };

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

% Test: typing of the fix point
type(#call{ target=Target=#name{name=fix }, arguments=[Closure]} = Expr) ->

    #closure{ lambda=Lambda }  = Closure,
    #lambda{ definitions= [ NameDef = #nameDef{ name=SelfReference } | _Other ] } = Lambda,
    Type = #typeCl{
           lambda = #typeFun{ return=int, arguments=[int]},
            name   = "clojure_fix",
            captures=[] %#typeFun{ return=int, arguments=[int]}
        },
    %Type = #typeFun{return=int, arguments=[int]},
    emit_closure_struct(Type),
    compil_table:setType(SelfReference, Type),
    TypedClosure = type(Closure),
    #fix{ closure=TypedClosure, type=#typeFun{return=int, arguments=[int]}};


type(#call{ target=Target=#name{name='cond' }, arguments=[Cond, Left, Right]} = Expr) ->
    TypedCond = ensure_type(Cond,bool),
    TypedLeft = type(Left),
    ExpectedType = extract_type(Left),
    TypedRight = ensure_type(Right, ExpectedType),
    TypedTarget = Target#name{ type=#typeFun{ arguments = [ bool, ExpectedType, ExpectedType ], return = ExpectedType}},
    Expr#call{ type=ExpectedType, target=TypedTarget, arguments = [ TypedCond, TypedLeft, TypedRight ]};


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
extract_type(#closure{ type= Type}) -> Type;
extract_type(#fix{ type= Type}) -> Type.


% @doc: show which type may be mapped to int
%conversions(int,float) ->
%    float.

buildin_type(OP) ->
    case
        {   lists:member(OP,?BOOL_OPS),
            lists:member(OP,?CMP_OPS),
            lists:member(OP,?NUM_OPS)} of
        { true, _, _ } ->
            #typeFun{arguments=[bool, bool], return=bool};
        { _, true, _ } ->
            #typeFun{arguments=[int, int], return=bool};
        { _, _, true} ->
            #typeFun{arguments=[int, int], return=int}
    end.


-define(NOCODE,[]).
-define(LIN(Tokens), string:join(Tokens," ")).
-define(FUNCTION(Lines), hd(Lines) ++ "{\n\t" ++ string:join(tl(Lines),"\n\t") ++ "\n}\n").
-define(LABEL(Name),"\n" ++ tl(Name) ++ ":").

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



codegen(#call{ target=Target=#name{name='cond' }, arguments=[Cond, Left, Right]}) ->
    {CondVar, CondCodes } = codegen(Cond),
    {TrueVar, TrueCodes } = codegen(Left),
    {FalseVar, FalseCodes } = codegen(Right),

    LabelTrue = compil_table:newTemp(),
    LabelFalse = compil_table:newTemp(),
    LabelContinue = compil_table:newTemp(),

    Pointer = compil_table:newTemp(),
    MyVar = compil_table:newTemp(),

    % TODO: typing
    AllocCode = ?LIN([Pointer,"=","alloca","i32"]),
    BranchCode = ?LIN(["br i1",CondVar++",","label",LabelTrue ++ ",","label",LabelFalse  ] ),
    LabelTrueCode = ?LABEL(LabelTrue),
    StoreTrueCode = ?LIN(["store","i32",TrueVar++",","i32*",Pointer]),
    ContinueCode  = ?LIN(["br","label",LabelContinue]),
    LabelContinueCode = ?LABEL(LabelContinue),
    LabelFalseCode = ?LABEL(LabelFalse),
    StoreFalseCode = ?LIN(["store","i32",FalseVar++",","i32*",Pointer]),

    LoadCode = ?LIN([MyVar,"=","load","i32*",Pointer]),

    {MyVar, lists:reverse(lists:reverse(CondCodes) ++ [AllocCode, BranchCode] ++
            [LabelTrueCode | lists:reverse(TrueCodes)] ++
            [ StoreTrueCode | [ContinueCode | [LabelFalseCode | lists:reverse(FalseCodes)]]] ++
            [StoreFalseCode, ContinueCode,  LabelContinueCode, LoadCode ])};


codegen(#call{ type=RetType, target = Target,  arguments = Args }) ->

    CodeArgs = lists:map(fun(Arg) ->  codegen(Arg) end, Args),
    {Vars, ArgCodes}  = lists:unzip(CodeArgs),
    ArgCodeLines =  lists:concat(ArgCodes),
    case Target of
        #name{ name=Primitiv, primitiv=true } ->
            MyVar = compil_table:newTemp(),
            [VarA, VarB] = Vars,
            MyOp = ?LIN([MyVar,"=", decode(Primitiv), edt(hd(Args)), VarA,",",VarB]),
            {MyVar, [ MyOp | ArgCodeLines ]};
        #lambda{ type=#typeFun {arguments=ArgTypes} } = Lambda ->
            {LambdaVar, _ } = codegen(Lambda),
            MyVar = compil_table:newTemp(),
            ArgsStr = lists:map( fun({Var, Type}) -> decode_type(Type) ++ " " ++  Var end, lists:zip(Vars, ArgTypes)),
            VarString = string:join(ArgsStr,", "),
            MyCall = ?LIN([MyVar,"=","call",decode_type(RetType), LambdaVar, "(", VarString ,")"]),
            {MyVar, [ MyCall | ArgCodeLines ]};

    #fix{ closure=Closure } ->
        #closure{ lambda = Lambda =  #lambda{ type=Type}} = Closure,
        {FunctionName,_} = codegen(Lambda),
        StructPointer = compil_table:newTemp(),
        ClosureTypeName = "clojure_fix",
        ClosureType = "%" ++ ClosureTypeName ++"*",

        FixName =  "@fix",
        ValueName = "%fix_value",
        RetValueName = "%fix_ret",
        FixHeader = ?LIN(["define","i32",FixName,"(i32",ValueName,")"]),
        FixCall =  ?LIN([RetValueName,"=","call","i32",FunctionName,"(", ClosureType ,StructPointer,
                ",","i32",ValueName,")"]),
        FixReturn = ?LIN(["ret","i32",RetValueName]),

        ArgTypes = [int],
        ArgsStr = lists:map( fun({Var, Type}) -> decode_type(Type) ++ " " ++  Var end,
            lists:zip(Vars, ArgTypes)),
        VarString = string:join(ArgsStr,", "),


        FunctionPointer = compil_table:newTemp(),
        AllocCode = ?LIN([StructPointer,"=","alloca","%" ++ ClosureTypeName]),
        GetFPCode = ?LIN([FunctionPointer,"=","getelementptr",ClosureType,StructPointer++ ",","i32 0,","i32 0"]),
        StoFPCode = ?LIN(["store","i32(i32)*", FixName ++ "," ,"i32(i32)*" ++ "*",FunctionPointer]),

        Code = ?FUNCTION([ FixHeader, AllocCode, GetFPCode, StoFPCode, FixCall, FixReturn]),
        compil_emitter:function(Code),

        MyVar = compil_table:newTemp(),
        MyCall = ?LIN([MyVar,"=","call",decode_type(RetType), FixName, "(", VarString ,")"]),
        {MyVar, [ MyCall | ArgCodeLines ]};


        Other -> % callee must be a closure
            ClType = #typeCl{ lambda=FunType = #typeFun{ arguments=ArgTypes }, captures=CaptureTypes } = extract_type(Other),
            { StructPointer, PrevCodes} = codegen(Other),

            FunctionPointerPointer = compil_table:newTemp(),
            FunctionPointer = compil_table:newTemp(),

            ComputeFPPCode = ?LIN([FunctionPointerPointer,"=","getelementptr", decode_type(ClType),StructPointer ++ ",","i32 0,","i32 0"]),
            LoadFPCode = ?LIN([FunctionPointer,"=","load",decode_type(FunType)++"*",FunctionPointerPointer]),

            {BindVars, BindCodes} = lists:unzip(lists:map(
                fun
                    ({ Type, Num}) ->
                        BindPointer = compil_table:newTemp(),
                        Binding = compil_table:newTemp(),
                        NumStr = integer_to_list(Num),

                        ComputeBPCode = ?LIN([BindPointer,"=","getelementptr",decode_type(ClType),StructPointer ++ ",","i32 0,","i32",NumStr]),
                        LoadBCode = ?LIN([Binding,"=","load",decode_type(Type) ++ "*",BindPointer]),

                        {Binding ,[ComputeBPCode, LoadBCode]}

                end,
                lists:zip(CaptureTypes,lists:seq(1, length(CaptureTypes))))),
            BindCode = lists:concat(BindCodes),

            ArgsStr = lists:map( fun({Var, Type}) -> decode_type(Type) ++ " " ++  Var end, lists:zip(Vars ++ BindVars, ArgTypes)),
            VarString = string:join(ArgsStr,", "),
            MyVar = compil_table:newTemp(),
            CallCode = ?LIN([MyVar,"=","call",decode_type(RetType),FunctionPointer,"(",VarString,")"]),

            { MyVar, lists:reverse(BindCodes ++ [ComputeFPPCode,LoadFPCode,CallCode]) ++ PrevCodes ++
            ArgCodeLines }

    end;

codegen(#name{ name=Var}) ->
    { "%" ++ Var, ?NOCODE};


codegen(#closure{ lambda = Lambda = #lambda { type=FunType },type = ClType,  captures= Captures}) ->
    { FunctionName, _ } = codegen(Lambda),
    % Typedef := { Pointer to Function, Number of Bindings, Binding }
    StructPointer = compil_table:newTemp(),
    FunctionPointer = compil_table:newTemp(),

    #typeCl{name=ClosureTypeName} = ClType,
    ClosureType = decode_type(ClType),

    FunTypeStr = decode_type(FunType),

    emit_closure_struct(ClType),

    AllocCode = ?LIN([StructPointer,"=","alloca","%" ++ ClosureTypeName]),
    GetFPCode = ?LIN([FunctionPointer,"=","getelementptr",ClosureType,StructPointer++ ",","i32 0,","i32 0"]),
    StoFPCode = ?LIN(["store",FunTypeStr, FunctionName ++ "," ,FunTypeStr ++ "*",FunctionPointer]),

    CaptureCode = lists:concat(lists:map(
        fun
            ({ #name{name=CaptureName, type=CaptureType }, Num}) ->
                TypeStr = decode_type(CaptureType),
                BindPointer = compil_table:newTemp(),
                NumStr = integer_to_list(Num),
                GetBPCode = ?LIN([BindPointer,"=","getelementptr",ClosureType,StructPointer++",","i32 0,","i32",NumStr]),
                StoBPCode = ?LIN(["store",TypeStr,[$\%|CaptureName] ++ ",", TypeStr ++ "*",BindPointer]),
                [GetBPCode, StoBPCode]
        end,
        lists:zip(Captures,lists:seq(1, length(Captures))))),

    {StructPointer, lists:reverse([AllocCode, GetFPCode, StoFPCode] ++ CaptureCode)};




codegen(#literal{type=int, value=Val}) ->
    MyVar = integer_to_list(Val),
    {MyVar, ?NOCODE};
codegen(#literal{type=bool, value=Val}) ->
    MyVar = case Val of
        true ->
            "1";
        false ->
            "0"
    end,
    {MyVar, ?NOCODE}.

emit_closure_struct(#typeCl{ name=ClosureTypeName, lambda=TypeFun, captures=CaptureTypes }) ->
    ArgTypes = lists:map(fun decode_type/1,CaptureTypes),
    FunString = string:join([decode_type(TypeFun) | ArgTypes],", "),
    compil_emitter:function(?LIN(["%" ++ ClosureTypeName,"=","type","{",FunString,"}"])).

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
decode('-') -> "sub";
decode('or') -> "or";
decode('and') -> "and";
decode('=') -> "icmp eq";
decode('<') -> "icmp slt";
decode('>') -> "icmp sgt".

% Types
decode_type(#typeFun{return=Return,arguments=Args}) ->
    decode_type(Return) ++ "(" ++ string:join(lists:map(fun decode_type/1, Args),", ") ++ ")*";
decode_type(#typeCl{ name=Name}) -> "%" ++ Name ++ "*";
decode_type(bool) -> "i1";
decode_type(int) -> "i32".
