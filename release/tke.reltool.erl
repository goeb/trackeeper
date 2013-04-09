{sys, [
       {lib_dirs, ["/home/fred/yaws-1.96-build/lib", "../lib"]},
       {rel, "tke-release", "1.0.0",
        [kernel,
         stdlib,
         {yaws, permanent},
         {tke, permanent}
        ]},
       {boot_rel, "tke-release"},
       {relocatable, true},
       {profile, standalone}
      ]}.
%% erl 
%% 1> {ok, Conf} = file:consult("tke.reltool.erl").
%% 2> {ok, Spec} = reltool:get_target_spec(Conf).
%% 3> reltool:eval_target_spec(Spec, code:root_dir(), "rel").
