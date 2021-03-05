-module(buildermanager).
-behaviour(gen_server).
-import(lists, [append/2]).
% Export at least the API:
-export([create/0, request/2, request_reply/2]).

-export([init/1, handle_call/3, handle_cast/2]).

%%%===================================================================
create() ->
    gen_server:start(?MODULE, [], []).
request(ServerRef, Request) ->
    gen_server:cast(ServerRef, Request).
request_reply(ServerRef, Request) ->
    gen_server:call(ServerRef, Request).

init([]) ->
    process_flag(trap_exit, true),
    Return = {ok, {[], [], [], none}},
    Return.

handle_call({build_manager, Plan}, _From, {ActionList, FunList, BuildList, Parent}) ->
  Pid = spawn(fun() -> builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, nothing, none) end), 
  Pid ! {start},
  NewBuildList = append(BuildList, [Pid]),
  {reply, {ok, Pid}, {ActionList, FunList, NewBuildList, Parent}};

handle_call({register_action, Act, Fun}, _From, {ActionList, FunList, BuildList, Parent}) ->
    case lists:member(Act, ActionList) of 
      true ->
          {reply, {error, already_defined}, {ActionList, FunList, BuildList, Parent}};
      false ->
          NewFunList = append(FunList, [{Act, Fun}]),
          NewActList = append(ActionList, [Act]),
          {reply, ok, {NewActList, NewFunList, BuildList, Parent}}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    Return = {reply, Reply, State},
    Return.

handle_cast({build, {Type, Plans}}, {ActionList, FunList, BuildList, Parent}) ->
  case Plans of
    [] -> Result = {failure, []};
    _ ->
      case Type of
        any -> Result = helper_funcAny(result, {ActionList, FunList, BuildList, none}, Plans);
        all -> Result = helper_funcAll(result, {ActionList, FunList, BuildList, none}, Plans);
        seq -> Result = helper_func(result, {ActionList, FunList, BuildList, none}, Plans);
        failure_is_success -> Result = helper_funcFIS(result, {ActionList, FunList, BuildList, none}, Plans)
      end
  end,
  if
    Parent /= none -> Parent ! Result;
    true -> ok
  end,
  {noreply, Result};

handle_cast({build, {within, Limit, Plan}}, {ActionList, FunList, BuildList, Parent}) ->
  case Plan of
    [] -> Result = {failure, []};
    _ -> Result = helper_funcWithin(result, {ActionList, FunList, BuildList, none}, Plan, Limit)
  end,
  if
    Parent /= none -> Parent ! Result;
    true -> ok
  end,
  {noreply, Result};

handle_cast({build, {and_then, Plan, FunBuild}}, {ActionList, FunList, BuildList, Parent}) ->
  case Plan of
    [] -> Result = {failure, []};
    _ -> Result = helper_funcAndThen(result, {ActionList, FunList, BuildList, none}, Plan, FunBuild)
  end,
  if
    Parent /= none -> Parent ! Result;
    true -> ok
  end,
  {noreply, Result};

handle_cast({build, {act, Act, Arg}}, {ActionList, FunList, _, Parent}) ->
  case lists:member(Act, ActionList) of 
  true ->
    Active = lists:keyfind(Act, 1, FunList),
    {_, Functi} = Active,
    Pid = spawn(fun() -> actionLoop(nothing, none) end),
    Pid ! {start, Functi, Arg},
    Ref = make_ref(),
    Pid ! {waitOn, Ref, self()},
    receive
      Value -> Value,
      if
        Parent /= none ->
        case Value of 
        {success,{success, NewVal}} -> Parent ! {success, NewVal};
        {failure,{failure, NewVal}} -> Parent ! {failure, NewVal};
        Otherwise -> Parent ! Otherwise
        end;
        true -> ok
      end,
      {noreply, Value}
    end;   
  false ->
    if
    Parent /= none -> Parent ! {failure, no_such_action};
    true -> ok
    end,
    {noreply, {failure, no_such_action}}
  end;

handle_cast(finish, {ActionList, FunList, BuildList, Parent}) ->
  lists:map(fun(L)-> exit(L, kill) end, BuildList),
  {stop, shutdown, {ActionList, FunList, BuildList, Parent}}.

actionLoop(State, ReceiverInfo) ->
    Me = self(),
    receive
      {start, Fun, Arg} -> 
          spawn(fun() -> try Fun(Arg) of N -> Me ! {success, N} catch _:Error -> Me ! {failure, Error} end end),
          actionLoop(nothing, none);
      {waitOn, Ref, From} ->
        if State /= nothing -> 
          case State of
            {success, Value} -> From ! {success, Value}, actionLoop(State, {Ref, From});
            {failure, Value} -> From ! {failure, Value}, actionLoop(State, {Ref, From});
            _ -> actionLoop(State, ReceiverInfo)
          end;
          true -> actionLoop(State, {Ref, From})
          end;        
      {success, Value} -> 
            case ReceiverInfo of
                {_, From} -> From ! {success, Value},
                    actionLoop({success, Value}, ReceiverInfo);
                _ -> actionLoop(State, ReceiverInfo)
            end;
      {failure, Reason} ->
            case ReceiverInfo of
                {_, From} -> From ! {failure, Reason},
                    actionLoop({failure, Reason}, ReceiverInfo);
                _ -> actionLoop(State, ReceiverInfo)
            end
    end.

helper_func(Result, _, []) ->
    Result;
helper_func(_, {ActionList, FunList, BuildList, Parent}, [Values|Tail]) ->
    {noreply, Value} = handle_cast({build, Values}, {ActionList, FunList, BuildList, none}),
    NewResult = Value,
    case Value of
        {success, _} -> helper_func(NewResult, {ActionList, FunList, BuildList, Parent}, Tail);
      _ -> helper_func(NewResult, {ActionList, FunList, BuildList, Parent}, [])
    end.

helper_funcAnyAll(Value, _, {ActionList, FunList, BuildList, Parent}) ->
  Me = self(),
  spawn(?MODULE, handle_cast, [{build, Value}, {ActionList, FunList, BuildList, Me}]),
  receive
    Valuet -> Valuet
  end,
  Parent ! {Me, Valuet}.

gather(Pids, []) ->
    receive
        {_, {success, Res}} -> NewResult = {success, Res},
          gather([], NewResult);
        {_, _} -> gather(Pids, [])
    end;
gather([], Result) ->
  case Result of 
    [] -> {failure, Result};
    {success, Val} -> {success, Val}
  end.

helper_funcAny(Result, {ActionList, FunList, BuildList, _}, Values) ->
    Me = self(),
    Pids = lists:map(fun(L) -> spawn(fun() -> helper_funcAnyAll(L, Result, {ActionList, FunList, BuildList, Me}) end) end , Values),
    gather(Pids, []).

abortAll(List) ->
  lists:map(fun(L)-> exit(L, kill) end, List).

gatherAll([Pid|T], Result) ->
  receive
    {Pid, {success, Res}} -> 
      NewResult = append(Result, [{sucess, Res}]),
      gatherAll(T, NewResult);
    {Pid, {failure, Res}} ->
      abortAll(T),
      NewResult = {failure, Res},
      gatherAll([], NewResult)
  end;
gatherAll([], Result) ->
  case Result of 
    {failure, Val} -> {failure, Val};
    {success, Val} -> {success, Val};
    Result -> {success, Result}
  end.

helper_funcAll(Result, {ActionList, FunList, BuildList, _}, Values) ->
    Me = self(),
    Pids = lists:map(fun(L) -> spawn(fun() -> helper_funcAnyAll(L, Result, {ActionList, FunList, BuildList, Me}) end) end , Values),
    gatherAll(Pids, []).

helper_funcFIS(_, {ActionList, FunList, BuildList, _}, Value) ->
    Me = self(),
    spawn(?MODULE, handle_cast, [{build, Value}, {ActionList, FunList, BuildList, Me}]),
    receive
      Valuet -> Valuet
    end,
    case Valuet of 
      {failure, Val} -> {success, Val};
      {success, Val} -> {failure, Val};
      _ -> {success, other_error}
    end.

helper_funcAndThen(_, {ActionList, FunList, BuildList, _}, Value, Function) ->
    Me = self(),
    spawn(?MODULE, handle_cast, [{build, Value}, {ActionList, FunList, BuildList, Me}]),
    receive
      Valuet -> Valuet
    end,
    case Valuet of 
      {failure, Val} -> {failure, Val};
      {success, Val} ->spawn(fun() -> try Function(Val) of N -> Me ! {success, N} catch _:Error -> Me ! {failure, Error} end end),
                  receive
                    {failure, _} -> {failure, not_a_fun_plan};
                    {success, Replied} -> {success, Replied}
                  end;
      _ -> {failure, other_error}
    end.

helper_funcWithin(_, {ActionList, FunList, BuildList, _}, Value, Limit) ->
    Me = self(),
    spawn(?MODULE, handle_cast, [{build, Value}, {ActionList, FunList, BuildList, Me}]),
    A = erlang:system_time(milli_seconds),
    receive
      Valuet -> B = erlang:system_time(milli_seconds), Valuet
    end,
    C = (B-A)*100,
    if C =< Limit -> Valuet;
      true -> {failure, limit_exceeded}
    end.

builderLoop(Plan, {ActionList, FunList, BuildList, Parent}, ReceiverInfo, Result) ->
  Me = self(),
  receive
    {start} -> 
        spawn(?MODULE, handle_cast, [Plan, {ActionList, FunList, BuildList, Me}]),
        builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, nothing, none);
    {success, Value} -> 
      builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, ReceiverInfo, {success, Value});
    {failure, Reason} ->
      builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, ReceiverInfo, {failure, Reason});
    {on_completion, Ref, From, Funct} -> 
      if Result /= none -> 
        case Result of
          {success, _} ->
            spawn(fun() -> try Funct(Result) of N -> From ! N catch _:Error -> From ! Error end end),
            builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, {Ref, From}, Result);
          _ ->
            From ! Result,
            builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, {Ref, From}, Result)
        end;
      true -> From ! {action_ongoing}, builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, {Ref, From}, Result)
      end;
    {status, From} ->
      case Result of
        {success, Value} ->
          From ! {success, Value},
          builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, ReceiverInfo, Result);
        {failure, aborted} ->
          From ! {failure, aborted},
          builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, ReceiverInfo, Result);
        {failure, Reason} ->
          From ! {failure, Reason},
          builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, ReceiverInfo, Result);
        none ->
          From ! {ongoing, 1},
          builderLoop({build, Plan}, {ActionList, FunList, BuildList, Parent}, ReceiverInfo, Result)
      end
  end.

