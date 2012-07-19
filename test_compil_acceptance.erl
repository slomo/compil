-module(test_compil_acceptance).

-include_lib("eunit/include/eunit.hrl").

run_examples_test() ->
    Examplefiles = filelib:wildcard("examples/*.lisp"),
    lists:map(fun(Filename) ->
            LLFilename = string:sub_word(Filename,1,$\.) ++ ".ll",
            TestFun = fun () ->
                    io:format("Testing " ++ Filename ++ "\n"),
                    compil:fromFileToFile(Filename,LLFilename),
                    RetString = os:cmd("lli " ++ LLFilename ++ " > /dev/null; echo -n $?"),
                    { Num, _Rest } =  string:to_integer(RetString),
                    Num end,
            ?assert( TestFun() == 5  )
    end, Examplefiles).
