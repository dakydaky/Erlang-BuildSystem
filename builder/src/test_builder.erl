-module(test_builder).

-import(builder, []).
-import(buildermanager, []).

% You are allowed to split your test code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_builder.

-export([test_all/0, test_everything/0]).
-export([]). % You may have other exports as well

test_all() ->
  TestsList = [
   {"Simple register", test_simple_register_action()},
   {"Start stop", test_simple_start_stop()},
   {"Simple register, on completion", test_simple_register_and_on_completion()},
   {"No such action", test_no_such_action()},
   {"Complex build 1", test_complex_build1()},
   {"Complex build 2", test_complex_build2()},
   {"Complex build 3", test_complex_build3()},
   {"Failure is success", test_failure_is_success()},
   {"And then test", test_and_then()},
   {"And then test with failure clause", test_and_then_failure()},
   {"Test within", test_within()},
   {"Test from the appendix", test_appendix()}
  ],
  io:format("Running grouped tests, result is: ~w~n", [evaluate(ok, error, TestsList)]).

evaluate(Pos, Neg, Funs) -> evaluate(Pos, Neg, 1, Funs, ok).
evaluate(_, _, _, [], State) -> State;
evaluate(Pos, Neg, Num, [{Name, Fun} | Funs], State) ->
  try Fun() of
    Pos -> 
      io:format("Test ~p: ~p passed~n", [Num, Name]),
      evaluate(Pos, Neg, Num + 1, Funs, State);
    Neg -> 
      io:format("Test ~p: ~p failed!~n", [Num, Name]),
      evaluate(Pos, Neg, Num + 1, Funs, bad_result);
    Other -> 
      io:format("Test ~p: ~p returned unexpected: ~p~n", [Num, Name, Other]), bad_result
  catch _:_ -> io:format("Test ~p: ~p terminated!", [Num, Name]), bad_test
  end.

test_simple_start_stop() -> 
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    Result = builder:finish(E),
    case Result of
      ok -> ok;
      _ -> error
    end
  end.

  test_simple_register_action() ->
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    builder:register_action(E, hey, fun _(X)  -> X+2 end),
    Result = builder:build(E, {act, hey, 2}),
    case Result of
      {ok, _} -> ok;
      _ -> error
    end
  end.

  test_simple_register_and_on_completion() ->
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    builder:register_action(E, test2, fun _(X)  -> X*2 end),
    {ok, H} = builder:build(E, {act, test2, 2}),
    %%We require sleep to complete the action
    timer:sleep(50),
    Result = builder:on_completion(H, fun _({success, X})  -> X+3 end),
    case Result of
      7 -> ok;
      _ -> error
    end
  end.

  test_no_such_action() ->
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    builder:register_action(E, test2, fun _(X)  -> X*2 end),
    {ok, H} = builder:build(E, {seq, [{act, hey, 2},{seq, [{act, hey, 3}, {act, ola, 2}]}]}),
    timer:sleep(50),
    Result = builder:status(H),
    case Result of
      {failure, no_such_action} -> ok;
      _ -> error
    end
  end.

  test_complex_build1() ->
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    builder:register_action(E, hey, fun _(X)  -> X*2 end),
    {ok, H} = builder:build(E, {seq, [{act, hey, 2},{any, [{act, hey, 3}, {act, ola, 2}]}]}),
    timer:sleep(50),
    Result = builder:status(H),
    case Result of
      {success, 6} -> ok;
      _ -> error
    end
  end.

  test_complex_build2() ->
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    builder:register_action(E, hey, fun _(X)  -> X*2 end),
    {ok, H} = builder:build(E, {seq, [{act, hey, 2},{failure_is_success, {any, [{act, hey, 3}, {act, ola, 2}]}}]}),
    timer:sleep(50),
    Result = builder:status(H),
    case Result of
      {failure, 6} -> ok;
      _ -> error
    end
  end.

  test_complex_build3() ->
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    builder:register_action(E, hey, fun _(X)  -> X*2 end),
    {ok, H} = builder:build(E, {any, [{act, hey, 2},{seq, [{act, hey, 3}, {act, ola, 2}]}]}),
    timer:sleep(50),
    Result = builder:status(H),
    case Result of
      {success, 4} -> ok;
      _ -> error
    end
  end.

  test_failure_is_success() ->
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    builder:register_action(E, hey, fun _(X)  -> X*2 end),
    {ok, H} = builder:build(E, {failure_is_success, {act, hey, 2}}),
    timer:sleep(50),
    Result = builder:status(H),
    case Result of
      {failure, 4} -> ok;
      _ -> error
    end
  end.

  test_and_then() ->
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    builder:register_action(E, hey, fun _(X) -> X*2 end),
    {ok, H} = builder:build(E, {and_then, {any, [{act, hey, 3}, {act, ola, 2}]}, fun _(X)-> X+5 end}),
    timer:sleep(50),
    Result = builder:status(H),
    case Result of
      {success, 11} -> ok;
      _ -> error
    end
  end.

  test_and_then_failure() ->
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    builder:register_action(E, hey, fun _(X) -> X*2 end),
    {ok, H} = builder:build(E, {and_then, {any, [{act, hey, 3}, {act, ola, 2}]}, fun _(X)-> X/0 end}),
    timer:sleep(50),
    Result = builder:status(H),
    case Result of
      {failure, not_a_fun_plan} -> ok;
      _ -> error
    end
  end.

  test_within() ->
  fun() -> 
    {ok, E} = builder:startup(),
    true = is_process_alive(E),
    builder:register_action(E, hey, fun _(X) -> X*2 end),
    {ok, H} = builder:build(E, {within, 10, {any, [{act, hey, 3}, {act, ola, 2}]}}),
    timer:sleep(50),
    Result = builder:status(H),
    case Result of
      {success, 6} -> ok;
      _ -> error
    end
  end.

  return(Arg) -> {success, Arg}.
  test_appendix() ->
  fun() -> 
    {ok, B1} =builder:startup(),
    builder:register_action(B1, ret,fun return/1),
    {ok, B2} =builder:startup(),
    builder:register_action(B2, ret,fun return/1),
    
    builder:register_action(B2, forever,fun Loop(X) -> Loop(X) end),
    builder:register_action(B1, forever, fun Loop(X) -> Loop(X) end),
    Plan = {seq, [ {any, [ {act, forever, 1}, {act, ret, "one"}]}, {any, [ {act, ret, 2}, {act, ret, "two"}]}]},
    Me = self(),

    Report = fun(BR) -> fun(Result) -> Me ! {BR, case Result of {success, "two"} -> true;
                                                                {success, 2}     -> true;
                                                                _ -> false
                                                                end}
            end end,
    {ok, BR1} = builder:build(B1, Plan),
    timer:sleep(50),
    {ok, BR2} = builder:build(B2, Plan),
    timer:sleep(50), 
    builder:on_completion(BR1, Report(BR1)),
    builder:on_completion(BR2, Report(BR2)),
    R1 = receive 
      {BR2, Res} -> Res 
    end,
    R2 = receive
      {BR2, Res2} -> Res2 
    end,
    case R1 andalso R2 of
      true -> ok;
      _ -> error
    end
  end.


test_everything() ->
  test_all().