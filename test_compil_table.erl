-module(test_compil_table).

-include_lib("eunit/include/eunit.hrl").



env_test() ->

    compil_table:start_link(),

    Ref = make_ref(),
    ParentRef = make_ref(),

    Member1 = {a, aa},
    Member2 = {b, bb},

    compil_table:newEnv(Ref, ParentRef),
    compil_table:addMemberEnv(Ref, Member1),
    compil_table:addMemberEnv(Ref, Member2),

    [
        ?_assert( ParentRef =:= compil_table:getParentEnv(Ref)),
        ?_assert([Member2, Member1] =:= compil_table:getMemberEnv(Ref))
    ].
