-module(builder).

-import(buildermanager, []).
% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called builder.

% Export at least the API:
-export(
  [ startup/0
  , register_action/3
  , build/2
  , on_completion/2
  , status/1
  , finish/1]).

% You may have other exports as well
-export([]).


%%%===================================================================
%%% API
%%%===================================================================
startup() ->
  buildermanager:create().

register_action(Builder, Act, Fun) ->
  case is_process_alive(Builder) of
    true -> buildermanager:request_reply(Builder, {register_action, Act, Fun});
    false -> {error, not_alive}
  end.

build(Builder, Plan) ->
  case is_process_alive(Builder) of
    true -> buildermanager:request_reply(Builder, {build_manager, Plan});
    false -> {error, not_alive}
  end.

on_completion(BuildRef, Fun) ->
  case is_process_alive(BuildRef) of
    true ->
      Ref = make_ref(),
      Me = self(),
      BuildRef ! {on_completion, Ref, Me, Fun},
      receive
        Value -> Value
      end;   
    false -> {error, not_alive}
  end.

status(BuildRef) ->
  case is_process_alive(BuildRef) of 
    true ->
      Me = self(),
      BuildRef ! {status, Me},
      receive
        Message -> Message
      end;
    false -> {error, not_alive}
 end.

finish(Builder) ->
  case is_process_alive(Builder) of
    true -> buildermanager:request(Builder, finish);
    false -> not_alive
  end.
