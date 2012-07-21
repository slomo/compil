%% -----------------------------------------------------------------------------
%% @author Yves Müller <myves.addAnAt.zedat.fu-berlin.de>
%% @copyright Simplified BSD License by Yves Müller, 2012
%% @title Compil.erl
%% @doc A simple compiler for a lispy language
%% @version 0.0.1
%% -----------------------------------------------------------------------------

-module(compil).

-export([do/1, lli/1, doFile/1, fromFileToFile/2]).

-export([main/1]).

%% doc: this should be called as init, if running with no shell

main([]) ->
    io:fwrite("Need input file");
main([FromFile]) ->
    ?MODULE:doFile(atom_to_list(FromFile));
main([FromFile,ToFile]) ->
    ?MODULE:fromFileToFile(atom_to_list(FromFile),atom_to_list(ToFile));
main(Other)->
    io:write(Other).

%% == public  API =============================================================

%% doc: compile and print it
do(String) ->
    runAsProcess(String, fun compil_emitter:to_stdout/0 ).

%% doc: compile and try to run in lli
lli(String) ->
    runAsProcess(String, fun compil_emitter:to_lli/0).

%% doc: read from file compile and write to file
fromFileToFile(SourceFileName, TargetFileName) ->
    {ok, String} = file:read_file(SourceFileName),
    runAsProcess(binary_to_list(String),fun
        (_) -> compil_emitter:to_file(TargetFileName) end).

%% doc: read from file compile and put to stdout
doFile(SourceFileName) ->
    {ok, String} = file:read_file(SourceFileName),
    runAsProcess(binary_to_list(String),fun compil_emitter:to_stdout/0).

%% doc: wrapper to run compile in other process, in case it crashes. The wrapper
%%      starts and stops also needed services
runAsProcess(String, ResultHandler) ->
    {ok, _} = compil_table:start_link(),
    {ok, _} = compil_emitter:start_link(),
    Pid = self(),
    spawn(fun () ->  do(String, Pid) end),
    receive
        done ->
            Ret = ResultHandler();
        Value ->
            io:format("compiler returned: ~p",[Value]),
            Ret = Value
    after 2000 ->
            io:format("compiler crashed"),
            Ret = timeout
    end,
    compil_table:stop(),
    compil_emitter:stop(),
    Ret.

%% doc: compilation function, chains all steps together and notifies sender
do(String, Pid) ->
    % read all s-expr
    Trees = lexAndParse(String),

    % precompile them
    PrecompiledTrees = lists:map(fun precompile/1, Trees),

    % add global env (parent of uppermost symbols)
    compil_table:newEnv(global, undefined),
    EnvCodes = lists:map(fun(Tree) ->
                split_env(Tree, global) end, PrecompiledTrees),


    % scan for free vars
    lists:map(fun (Code) -> scan_free(Code, global) end, EnvCodes),

    % introduce clsoures
    ClosedCodes = lists:map(fun close_var/1, EnvCodes),

    % type it
    TypedCodes = lists:map(fun type/1, ClosedCodes),

    % generate llvm
    LLVM = lists:map(fun codegen/1, TypedCodes),

    % generate llvm for main and emit
    compil_emitter:function(maingen(LLVM) ++ "\n"),

    % signal caller
    Pid ! done.


%% == internal implementation =================================================

%% In the part of the module all basic steps of the compilation process are
%% implemented in the following code. The modul it self is stateless, but side
%% effects accour on the symbol table defined in compil_table.

%% -- Records and Types -------------------------------------------------------

%% This is the type of s-expressions, after there where read by the lfe parser

-type s_expr() :: atom() | integer() | [ s_expr() ].

%% Following are most of the record and type definitions used in this project.

%% These definitions conclude all primitive operators. A primary operator is a of
%% the source language, that has a direct representation in llvm.
-type primitive_operator() :: 'and' | 'or' | '=' | '<' | '>' |
                             '+' | '-' | '*' | '/'.
-define(BOOL_OPS,['and','or']).
-define(CMP_OPS,['=','<','>']).
-define(NUM_OPS,['+','-','*','/']).

-define(PRIMOPS,?BOOL_OPS ++ ?CMP_OPS ++ ?NUM_OPS).


%% Buildins are all primary operators, as well as all special keywords of the
%% target language.
-define(BUILDINS,?PRIMOPS ++ ['cond', fix]).


%% These records and types are used to express types in the compilation process
%% later on. They are defined here so that all definitions are in one place.

%% This is a function type, in llvm represented by the function pointer:
%% return(arg1,arg2 .. argn)*
-record(typeFun,{
        arguments = []      :: [ type_def() ],
        return              :: type_def()
    }).

%% This represents the type of a closure (passable function). In llvm it is
%% maped to a record, with the namen given in this type. This records has as
%% first element a pointer of type lambda, and then the captures following.
-record(typeCl,{
        lambda              :: #typeFun{},
        name                :: string(),
        captures =[]        :: [ type_def() ]
    }).

-type basic_type_def() :: int | bool.
-type type_def() :: basic_type_def() | #typeFun{} | #typeCl{}.


%% The following definitons compose the ast, which all compile functions work
%% on. As mentioned additional data might be stored in the symbol table, but
%% most is stored here.

-record(literal, {
        type                :: type_def(),
        value               :: boolean() | integer()
    }).

-record(name,{
	    name                :: string(),
	    env                 :: reference(), % env which defines that name
        type                :: type_def(),
        buildin             :: boolean(),
        primitiv            :: boolean()
    }).

%% This record is used, where new names are defined, i would not use it again,
%% becaus all data can be keept in a #name{} record, an wether it is a
%% definiton or not is clear from the context
-record(nameDef, {
        oldName             :: atom(),      % name from source code
        name                :: string(),    % unique name in programm
        type                :: type_def()
    }).

-type env_ref() :: reference() | global.
-record(lambda, {
	  ref                   :: env_ref(), % unique reference for symbol table
      label                 :: string(),    % name in llvm code
      type                  :: #typeFun{},
      definitions           :: #nameDef{},  % names introduced by this functions
      free_vars,                            % { var name, new boundname }
      child                 :: ast()
	 }).


-record(closure,{
        lambda              :: #lambda{},  % anonymous function
        type                :: #typeCl{},
        captures = []       :: [ #name{} ] % names captured from outer env
    }).

-record(fix,{
        closure             :: #closure{}, % recursiv function
        type                :: #typeFun{}
    }).

-record(call,{
        target              :: #name{} | #typeFun{} | #closure{} | #fix{},
	    arguments           :: [ ast() ],
        type                :: type_def()
	 }).

%% ast can be defined without #nameDef{} => #nameDef useless
-type ast() :: #lambda{} | #closure{} | #call{} | #fix{} | #name{} | #literal{}.


%% -- the lfe-parser wrapper ---------------------------------------------------
%% As mentioned the parser is borrowed from lfe, here a simple wrappe

-spec lexAndParse(string()) -> [ s_expr() ].
% returns list of all s_expr in that string 
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


%% -- the precompiler ---------------------------------------------------------
%% The precompiler makes simple transformations on s-exprs

-spec precompile(s_expr()) -> s_expr().

% for +,-,*,/ fold if there are more that to arguments
% eg: ( + 1 2 3 ) -> ( + 1 ( + 2 3 ) )
precompile([ Op | [ Arg1 | Args ]])
    when Op =:= '+'; Op =:= '-'; Op =:= '/'; Op =:= '*' ->
    lists:foldl(
        fun(X,Y) ->  [ Op , Y , precompile(X)] end,
        precompile(Arg1),
        Args);

% replace let with lambda
precompile([ 'let' , Definitions, Child]) ->
    { FormalArguments, Parameters } = lists:unzip(lists:map( fun erlang:list_to_tuple/1, Definitions)),
    [
        [   lambda ,
            lists:map(fun precompile/1, FormalArguments),
            precompile(Child)
        ]
        | lists:map(fun precompile/1, Parameters) ];

% otherwise decent
precompile(List) when is_list(List) ->
    lists:map(fun  precompile/1,List);
precompile(Other) ->
    Other.


%% -- the environment analysis.................................................

%% @doc:    Split_env performs the environment analysis. Wich means that all
%%          lambdas are discovered and entered into the env table, and all names
%%          are altered so that they are unique. In order to full fill this task,
%%          split_env, transforms all s-exprs to ast elements (not so clean)

-spec split_env(s_expr(),env_ref()) -> ast().

%% store in env table (with reference to parent), rename all defintions
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

%% process calls
split_env([ Function | Arguments], Env) ->
    Target = split_env(Function, Env),
    ScannedArguments = lists:map(fun (Thing) ->  split_env(Thing,Env) end, Arguments),
    #call{ target = Target, arguments = ScannedArguments};

%% litereals: just transform to ast
split_env(IntLit, _Env) when is_integer(IntLit) ->
    #literal{ type=int, value=IntLit };
split_env(BoolLit, _Env) when is_boolean(BoolLit) ->
    #literal{ type=bool, value=BoolLit};

%% names: test if buildin or primitv and produce ast, if not get new name
split_env(Atom, Env) when is_atom(Atom) ->
    case {lists:member(Atom,?BUILDINS),lists:member(Atom,?PRIMOPS)} of
        {false, false}  ->
            {NameEnv, NewName} = lookup_name(Env, Atom),
            #name{name = NewName, env = NameEnv, buildin=false };
        {IsBuildin, IsPrimitiv} ->
            #name{name=Atom, buildin=IsBuildin, primitiv=IsPrimitiv}
    end.

%% no strings jet
%split_env(StringLit) when is_string(StringLit) ->
%    #literal{ type=string, value=StringLit };


%% @doc:    lookup_name crawls the env table, to find the new name of a
%%          variable and the environment (lambda) it is defined in.

-spec lookup_name(env_ref(), atom()) ->  { env_ref(), string() }.

lookup_name(Env, Name) ->
    Members  = compil_table:getMembersEnv(Env),
    case proplists:lookup(Name, Members) of
        {Name,NewName} ->
            {Env, NewName};
        none when Env /= global ->
            lookup_name(compil_table:getParentEnv(Env), Name);
        _ ->
            throw("Error name not found")
    end.


%% -- The Closure Building ----------------------------------------------------

%% @doc:    scan_free looks for free variables in all lambdas. the information
%%          gathered in this step is not represented in the ast, it is only
%%          stored in the env table.

-spec scan_free(ast(), env_ref()) -> done.

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

%% @doc:    while scan_free only looks for names, it calls this method, if it
%%          has found one. add_free_var goes via the env_table through the env
%%          and all its parents and marks in that table, wether the name is
%%          free in that environment

-spec add_free_var(env_ref(), string()) -> done.

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


%% @doc:    this functions closes all free values, by going through the ast,
%%          and adding additional formal parameters to lambdas as needed.
%%          Together with scan_free it performs the lambda lifting algorithm.

-spec close_var(ast()) -> ast().

%% Here direct calls to lambdas are handeled special, that patern accours later
%% on. It optimises direct calls, but makes handling a lot more complex. The
%% optimisation is, that no closure is build, so this can be implemented by a
%% direct llvm call.
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
    Call#call{
        target =  close_var(Target),
        arguments = lists:map( fun close_var/1, Args)
    };

% If the lambda is not directly called transfrom it into a closure (introduces
% closure into ast.
close_var(Lambda = #lambda{ ref=Ref, definitions=Defs, child=Child}) ->
    NewChild = close_var(Child),
    Free = compil_table:getFreeEnv(Ref),
    FreeDefs = lists:map(fun(Name) -> #nameDef{name=Name} end, Free),
    NewLambda = Lambda#lambda{definitions = Defs ++ FreeDefs, child=NewChild},
    FreeArguments = lists:map(fun(Var) -> #name{name=Var} end, Free),
    #closure{ lambda=NewLambda, captures = FreeArguments };

close_var(Other) ->
    Other.


%% -- The typeinference -------------------------------------------------------
%% a not so clever type guessing game


-spec type(ast()) -> ast().

type(#closure{lambda=Lambda, captures=Captures} = Closure) ->
    TypedLambda = type(Lambda),
    {TypedCaptures, CaptureTypes}  = lists:unzip(lists:map(
        fun
            (#name{ name=Name}=Def) ->
                Type = compil_table:getType(Name),
                {Def#name{type=Type},Type}
        end, Captures)),
    Closure#closure{ type=#typeCl{
            name= "clojure_" ++ Lambda#lambda.label, %closure record name
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

%% Until now non primitive build ins (like cond and fix) operators underlied
%% the same rules, but now we must capture calls to them (kind of ugly, next
%% time i would extract them earlier).

%% typing for fix:
%%  * guess return type of recursiv function
%%  * help the type inference for the function
%%  * name fix it self callable, with type of the function
type(#call{ target=Target=#name{name=fix }, arguments=[Closure]} = Expr) ->

    % FIXME: i am evil fixed typed code
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

%% typing for cond:
%%  * ensure that cond is boolean
%%  * get type of second expr
%%  * ensure that third has same type
%%  * set cond to that type
type(#call{ target=Target=#name{name='cond' }, arguments=[Cond, Left, Right]} = Expr) ->
    TypedCond = ensure_type(Cond,bool),
    TypedLeft = type(Left),
    ExpectedType = extract_type(Left),
    TypedRight = ensure_type(Right, ExpectedType),
    TypedTarget = Target#name{ type=#typeFun{ arguments = [ bool, ExpectedType, ExpectedType ], return = ExpectedType}},
    Expr#call{ type=ExpectedType, target=TypedTarget, arguments = [ TypedCond, TypedLeft, TypedRight ]};

%% generic calls:
%%  * get expected arguments and return type of the target
%%  * ensure all arguments are corrected typed
%%  * set call to return the retrun type of the target
type(#call{ target=Target, arguments=Arguments} = Call ) ->
    TypedTarget = type(Target),
    case extract_type(TypedTarget) of
        #typeFun{ return=ReturnType, arguments=ExpectedArgTypes} ->
            TypedArguments = type_check_args(Arguments, ExpectedArgTypes);
        #typeCl{ captures=CaptureTypes, lambda = #typeFun{ return=ReturnType, arguments=ExpectedArgTypes} } ->
            TypedArguments = type_check_args(Arguments, lists:subtract(ExpectedArgTypes, CaptureTypes))
    end,
    Call#call{ target = TypedTarget, arguments=TypedArguments, type=ReturnType};

%% all other buildins have a fixed type
type(#name{name=Name, buildin=true} = Expr) ->
    Expr#name{type=buildin_type(Name)};
%% type of name was hopefully findout before
type(#name{name=Name} = Expr) ->
    Type = compil_table:getType(Name),
    Expr#name{type=Type};
%% type already set
type(#literal{} = Lit) ->
    Lit.

%% doc: this is helper for typing calls, that checks, wether the expectation of
%%      of the call target are meet by the given args.

-spec type_check_args(ast(),type_def()) -> [ast()].

type_check_args(Got, Expected) ->
    length(Got) == length(Expected) orelse throw(invalid_number_of_arguements),
    lists:map(
        fun
            ({Argument, ExpectedType}) ->
                ensure_type(Argument, ExpectedType)
        end,lists:zip(Got, Expected)).

%% doc: ensure_type is a special kind of type function, it not only guesses
%%      types, it also sets types for variables, if they havent already one,
%%      and throws an exception if types do not match. It is the core of type
%%      inference.

-spec ensure_type(ast(),type_def()) -> ast().

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

%% doc: extract_type is helper, that simply unpacks the type of various ast
%%      elements.

-spec extract_type(ast()) ->  type_def().

extract_type(#call{ type = Type}) -> Type;
extract_type(#name{type = Type}) -> Type;
extract_type(#literal{type = Type}) -> Type;
extract_type(#lambda{type = Type}) -> Type;
extract_type(#closure{ type= Type}) -> Type;
extract_type(#fix{ type= Type}) -> Type.


%% doc: buildin_type shows wich types are expected by builins (mostly
%%      primitives). Since there is no float yet this is easy. Then we
%%      need more complex type inference. Also this function should be
%%      renamed to primitive_type.

-spec buildin_type(primitive_operator()) -> type_def().

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

%% -- The LLVM IR Generator ---------------------------------------------------
%% This is by far the ugliest code of the compiler. It cloud be improved by
%% seperating out routines for llvm genereation. But I had not enough time.


%% These are some little helpers defined as macros. God know why I choose macros
%% to do this. They help for some basic tasks when emitting code.
-define(NOCODE,[]).
% since there is alread a std. macro named line I had to rename this fellow
-define(LIN(Tokens), string:join(Tokens," ")).
-define(FUNCTION(Lines), hd(Lines) ++ "{\n\t" ++ string:join(tl(Lines),"\n\t") ++ "\n}\n").
-define(LABEL(Name),"\n" ++ tl(Name) ++ ":").


%% doc: expcept for a handfull of helpers this is the only function for code
%%      generation, and it is a monster. It takes a ast node and transforms it
%%      to code. If needed the code is sent to the emitter (record types and
%%      completed inner functions). It returns the llvm var name, that holds
%%      the result of the evaluation of the ast, and code fragments to compute
%%      this result in reversed order (because appeding at the end of lists,
%%      is inefficient).

-spec codegen(ast()) -> {string(), [string()]}. % { varname, codefragements }

%% generate function code
%%  * emit global llvm function ( there are only global function in llvm)
%%  * return only name of that function
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
    {"@" ++ Label,?NOCODE};

%% standard if-then-else style code
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

%% calls are handeled in one clause with cases
codegen(#call{ type=RetType, target = Target,  arguments = Args }) ->

    %% common stuff for calls to all targets  (produce code for args)
    CodeArgs = lists:map(fun(Arg) ->  codegen(Arg) end, Args),
    {Vars, ArgCodes}  = lists:unzip(CodeArgs),
    ArgCodeLines =  lists:concat(ArgCodes),
    case Target of
        %% calls to build in primitives
        #name{ name=Primitiv, primitiv=true } ->
            MyVar = compil_table:newTemp(),
            [VarA, VarB] = Vars,
            MyOp = ?LIN([MyVar,"=", decode(Primitiv), edt(hd(Args)), VarA,",",VarB]),
            {MyVar, [ MyOp | ArgCodeLines ]};

        %% direct calls to lambdas (more efficient than producing a closure)
        #lambda{ type=#typeFun {arguments=ArgTypes} } = Lambda ->
            {LambdaVar, _ } = codegen(Lambda),
            MyVar = compil_table:newTemp(),
            ArgsStr = lists:map( fun({Var, Type}) -> decode_type(Type) ++ " " ++  Var end, lists:zip(Vars, ArgTypes)),
            VarString = string:join(ArgsStr,", "),
            MyCall = ?LIN([MyVar,"=","call",decode_type(RetType), LambdaVar, "(", VarString ,")"]),
            {MyVar, [ MyCall | ArgCodeLines ]};

        %% call to fix point operators (complex stuff ;)
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

        %% calls to other objects ( names, results of other calls etc)
        Other ->
            %% target mus be a closure
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

%% var names have already been transformed
codegen(#name{ name=Var}) ->
    { "%" ++ Var, ?NOCODE};

%% produce code for the definiton of closure ( capture environment etc ...)
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



%% handle literals
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

%% @doc emit a closure struct, that can be stored on the heap later.
%%      BEWARE: evil side effects!!

-spec emit_closure_struct(#typeCl{}) -> done.

emit_closure_struct(#typeCl{ name=ClosureTypeName, lambda=TypeFun, captures=CaptureTypes }) ->
    ArgTypes = lists:map(fun decode_type/1,CaptureTypes),
    FunString = string:join([decode_type(TypeFun) | ArgTypes],", "),
    compil_emitter:function(?LIN(["%" ++ ClosureTypeName,"=","type","{",FunString,"}"])),
    done.

%% @doc genereatre a main method from all top level code fragments

-spec maingen([{string(),[string()]}]) -> string().

maingen(Exprs) ->
    Final = lists:last(Exprs),

    % these are just emitted for thier side effects, but we have no side effects
    % sense is questionabel
    {_, Code} = lists:unzip(Exprs),
    Lines = lists:concat(lists:map(fun lists:reverse/1, Code)),
    {FinalVar, FinalCode} = Final,

    % other return types than int make no sense
    RetType = decode_type(int),

    ?FUNCTION(
            [ ?LIN([ "define", RetType, "@main", "()"]) | Lines ] ++
            [ ?LIN(["ret", RetType, FinalVar]) ]
    ).


% @doc: a bunch of helpers, that trans forms compiler internal structure 1:1
%       to llvm representations.

-spec decode(primitive_operator) -> string().

decode('+') -> "add";
decode('/') -> "div";
decode('*') -> "mul";
decode('-') -> "sub";
decode('or') -> "or";
decode('and') -> "and";
decode('=') -> "icmp eq";
decode('<') -> "icmp slt";
decode('>') -> "icmp sgt".

-spec decode_type(type_def()) -> string().

decode_type(#typeFun{return=Return,arguments=Args}) ->
    decode_type(Return) ++ "(" ++ string:join(lists:map(fun decode_type/1, Args),", ") ++ ")*";
decode_type(#typeCl{ name=Name}) -> "%" ++ Name ++ "*";
decode_type(bool) -> "i1";
decode_type(int) -> "i32".

% shortcut

-spec edt(ast()) -> string().
edt(Expr) -> decode_type(extract_type(Expr)).
