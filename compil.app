{application, compil,
    [
        {description,   "compil.erl is an compiler to llvm" },
        {id,            "compil"},
        {vsn,           "0.0.1"},
        {modules,       [ compil, compil_table, compil_emitter] },
        {maxT,          40000},
        {registered,    [compil_table, compil_emitter]},
        {env,           [
                            {o,"out.ll"}
                        ]},
        {mod,           {compil, []}}
    ]}.
