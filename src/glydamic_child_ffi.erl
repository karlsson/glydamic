-module(glydamic_child_ffi).

-include("../../gleam_otp/include/gleam@otp@supervision_ChildSpecification.hrl").
-include("../../gleam_otp/include/gleam@otp@supervision_Worker.hrl").
-include("../../gleam_otp/include/gleam@otp@actor_Started.hrl").
-include("../../glydamic/include/glydamic@child_SupervisedChild.hrl").

-export([start_child/2, start_child_callback/1, delete_child/2, restart_child/2,
         terminate_child/2, erl_child_spec/1]).

-spec start_child(SupPid :: pid(), #child_specification{}) ->
                     {ok, #supervised_child{}} | {error, term()}.
start_child(SupPid, GleamChildSpec) ->
    ErlChildSpec = #{id := Id} = erl_child_spec(GleamChildSpec),
    case supervisor:start_child(SupPid, ErlChildSpec) of
        {ok, _Pid, S} ->
            {ok, #supervised_child{id = Id, started = S}};
        {error, Error} ->
            {error, Error}
    end.

%%   Callback used by the Erlang supervisor module.
start_child_callback(StartFun) ->
    case StartFun() of
        {ok, #started{pid = Pid, data = _Data} = S} ->
            {ok, Pid, S};
        {error, Error} ->
            {error, {actor, Error}}
    end.

delete_child(SupPid, Id) ->
    case supervisor:delete_child(SupPid, Id) of
        ok ->
            {ok, nil};
        {error, Error} ->
            {error, Error}
    end.

restart_child(SupPid, Id) ->
    case supervisor:restart_child(SupPid, Id) of
        {ok, _Pid, S} ->
            {ok, #supervised_child{id = Id, started = S}};
        {error, Error}
            when Error == running;
                 Error == restarting;
                 Error == not_found;
                 Error == simple_one_for_one ->
            {error, Error};
        {error, _Error} ->
            {error, unknown}
    end.

terminate_child(SupPid, Id) ->
    case supervisor:terminate_child(SupPid, Id) of
        ok ->
            {ok, nil};
        {error, _Error} ->
            {error, not_exist}
    end.

%% Internal functions
%% Reserve first 2000 for indexed static processes.
unique_positive_integer() ->
    erlang:unique_integer([positive]) + 2000.

erl_child_spec(#child_specification{start = Start,
                                    child_type = ChildType,
                                    restart = Restart,
                                    significant = Significant}) ->
    MFA = {glydamic_child_ffi, start_child_callback, [Start]},
    {Type, Shutdown} =
        case ChildType of
            #worker{shutdown_ms = MS} when MS > 0 ->
                {worker, MS};
            #worker{} ->
                {worker, infinity};
            _ ->
                {supervisor, infinity}
        end,
    Id = unique_positive_integer(),
    #{id => Id,
      start => MFA,
      restart => Restart,
      significant => Significant,
      type => Type,
      shutdown => Shutdown}.
