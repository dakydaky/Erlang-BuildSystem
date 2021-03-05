-module(minimal).
-export([build_it/0]).

return(Arg) -> {success, Arg}.

build_it() ->
  {ok, B1} = builder:startup(),
  builder:register_action(B1, ret, fun return/1),
  {ok, B2} = builder:startup(),
  builder:register_action(B2, ret, fun return/1),

  builder:register_action(B2, forever, fun Loop(X) -> Loop(X) end),
  builder:register_action(B1, forever, fun Loop(X) -> Loop(X) end),

  Plan = {seq, [ {any, [ {act, forever, 1}
                       , {act, ret, "one"}]}
               , {any, [ {act, ret, 2}
                       , {act, ret, "two"}]}
               ]},

  Me = self(),

  Report = fun (BR) -> fun (Result) ->
             Me ! {BR, case Result of
                         {success, "two"} -> true;
                         {success, 2}     -> true;
                         _ -> false
                       end}
           end end,

  {ok, BR1} = builder:build(B1, Plan),
  {ok, BR2} = builder:build(B2, Plan),
  builder:on_completion(BR1, Report(BR1)),
  builder:on_completion(BR2, Report(BR2)),

  R1 = receive {BR1, Res} -> Res end,
  R2 = receive {BR2, Res2} -> Res2 end,

  R1 andalso R2.
