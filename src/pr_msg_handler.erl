%% PR_Msg_Handler.erl
%%
%% @copyright 2011-2012 The EMP Group <http://www.emp-d.com/>
%% @end
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either 
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library.  If not, see <http://www.gnu.org/licenses/>.
%% ----------------------------------------------------------------------------
%% @see plugrunner. <em>Plug Runner</em>
%% @see emp_plugin. <em>EMP Plugin Behaviour</em>
%%
%% @doc 
%%  This module is the message handling portion of Plug Runner. This handles 
%%  commands coming from interfaces and EMP itself, as well as messages coming
%%  from the plugin module its monitoring via the plugin API.
%% @end
%% ----------------------------------------------------------------------------
-module(pr_msg_handler).

-include("empinternal.hrl").
-include("pr_state.hrl").

% Export the Message handling function.
-export([handle_message/2]).

% Types of message that PlugRunner knows how to handle.
-type prmsg() :: plugmsg() | externmsg() | linkmsg() | internmsg().

% The result of the handle_message/2 command. It will either tell the PlugRunner
% that its time to shutdown, or that there was a change or no change to state
% based on what the message contains.
-type handle_result() :: no_change | shutdown | final_shutdown | #prstate{}.

% Plugin Messages are messages coming from the Plugin API via emp_plugin module.
-type plugmsg() :: {plugin, event, Name :: binary(), ValueList :: [ any() ] }
                 | {plugin, get_var, ModulePid :: pid(), VarName :: binary()}
                 | {plugin, set_var, async, ResponsePid :: pid(), VarName :: binary(), Value :: any()}
                 | {plugin, set_var, ResponsePid :: pid(), VarName :: binary(), Value :: any()}
                 | {plugin, update_var, ResponsePid :: pid(), VarName :: binary(), ApplyFun :: fun()}
                 | {plugin, shutdown, normal} 
                 | {plugin, shutdown, LoggableReason :: binary()}.

-type externmsg() :: {external, run_cmd, async, CmdName :: binary(), Params :: [ any() ]}
                   | {external, run_cmd, ReturnPid :: pid(), CmdName :: binary(), Params :: [ any() ]}. 

% These messages are from various links 
-type linkmsg() :: {'EXIT', From :: pid(), Reason :: atom()}.

% These messages are from PlugRunner itself for altering system state.
-type internmsg() :: {internal, shutdown} 
                   | {internal,final_shutdown}
                   | {internal, link_pids, PidList :: [pid()]}. 

%%%
%%% ===========================================================================
%%% Plug Runner's Message Handler
%%% ===========================================================================
%%%

%% @doc Handle a plug runner message.
-spec handle_message( #prstate{}, prmsg() ) -> handle_result().
handle_message( State, {plugin, event, EventName, ValueList} ) ->
    eman:trigger_event( State#prstate.user, 
                        State#prstate.plugin_module,
                                   EventName, 
                                   ValueList),
    no_change;

handle_message( State, {plugin,get_var, Pid, VarName} ) ->
    Val = 
        case lists:keysearch(VarName, 1, State#prstate.plugin_state) of 
            {value, {VarName, Value}} -> {ok, Value}; 
            _ -> {error, badarg}
        end,
    Pid ! Val, 
    no_change;

handle_message( State, {plugin,set_var, async, _Pid, Var,Val} )->
    State#prstate{plugin_state=lists:keystore(Var, 1, State#prstate.plugin_state, {Var, Val})};

handle_message( State, {plugin,set_var, Pid, Var, Val} )->
    try
        PlugState = lists:keystore(Var, 1, State#prstate.plugin_state, {Var, Val}),
        Pid ! {ok, Val},
        State#prstate{plugin_state=PlugState}
    catch 
        _:Res -> 
            Pid ! {error, Res}, 
            no_change 
    end;

handle_message( State, {plugin,update_var,Pid,VarName,ApplyFun} )->
    case lists:keysearch(VarName, 1, State#prstate.plugin_state) of
        {value, {_,Value}} -> 
            try
                NewVal = apply(ApplyFun, [Value]),
                Pid ! {ok, NewVal},
                State#prstate{plugin_state=lists:keystore(VarName, 1, 
                                                          State#prstate.plugin_state, 
                                                          {VarName,NewVal})}
            catch _: Res ->
                emplog:debug("Problem applying function in update_var: ~p", [Res]),
                Pid ! {error, Res}, 
                no_change
            end;
        false -> 
            emplog:debug("Unable to find variable ~p in plugin ~p",
                            [VarName,State#prstate.plugin_module]),
            Pid ! {error, badarg}, 
            no_change
    end;

handle_message( State, {external, run_cmd, async, CmdName,Params} ) ->
    StatePid = self(),
    %DON'T HALT PR WHILE PLUGIN IS RUNNING COMMAND.
    spawn(fun() -> runCmd(StatePid, State, CmdName, Params) end), 
    no_change;

handle_message( State, {external, run_cmd, Pid, CmdName,Params} ) ->
    StatePid = self(),
    %DON'T HALT PR WHILE PLUGIN IS RUNNING COMMAND.
    spawn( fun() -> Pid ! runCmd(StatePid, State, CmdName, Params) end),
    no_change;
                        
handle_message( _State, {plugin, shutdown, normal} ) -> 
    shutdown;

handle_message( _State, {plugin, shutdown, Reason} ) ->
    emplog:start("Plugin shut down for an abnormal reason: ~p",[Reason]),
    shutdown;

handle_message( _State, {'EXIT', _, _} ) -> 
    shutdown; %EProc, or pluginmodule went down, we should too.

handle_message( _State, {internal, shutdown} ) -> 
    shutdown;

handle_message( _State, {internal,final_shutdown} ) -> 
    final_shutdown;

handle_message( _State, {internal, link_pids, PidList} ) -> 
    linker(PidList), 
    no_change;

handle_message( State, Msg ) -> % Unknown message.
    UName = State#prstate.user#user.name,
    Module = State#prstate.plugin_module,
    emplog:warn("Plugrunner(~p) for ~p; ~p",[Module, UName, Msg]),
    State.

%%%
%%% ===========================================================================
%%% Internal Functionality
%%% ===========================================================================
%%%


%% @hidden Run a command on the Plugin module. This function is run from a spawn.
% TODO: we may want to implement a timeout on the function call: exit(Pid, timeout). or timer:kill_after
runCmd(_StateId, RunnerState, <<"list-events">>, _Params) ->
    case empdb:get_events(RunnerState#prstate.plugin_id) of
        {error, _Reason} -> {ok, []};
        EventList -> 
            {ok, lists:map(fun(E) -> E#event.name end, EventList)}
    end;
runCmd(_StateId, RunnerState, <<"list-commands">>, _Params) ->
    Fun = fun( Cmd ) -> Cmd#command.name end,
    {ok, lists:map(Fun, RunnerState#prstate.commands)};
runCmd(StateId, RunnerState, CmdName, Params) -> 
    case 
        lists:keysearch(CmdName, #command.name, RunnerState#prstate.commands) 
    of
        false -> 
            {error, <<"Unknown Command">>};
        {value, Command} ->
            applyCmd( StateId, Command, Params, RunnerState )
    end.

%% @hidden Apply the command with the merged parameter list.
applyCmd( StateId, Command, Params, RunnerState ) ->
    try 
        % Grab our default parameter list.
        DefaultParams = 
            try Command#command.dparams
            catch _:_ -> [] end,
        % Merge it with what we got from command line (or other interface).
        case merge_params(DefaultParams, Params) of
            {ok, NewParams} ->
                % If it was a success, apply the function with these parameters
                {ok, apply( Command#command.func, [StateId|NewParams] )};
            Err -> 
                % Otherwise, return the error.
                Err
        end
    catch _:Res ->
        % We can hit this error if there was an issue merging parameters,
        % or if there was a problem with running the plugin command.
        emplog:warn("Was unable to run ~p:~p command for ~p: ~p",
                        [RunnerState#prstate.plugin_module,
                         Command#command.name,
                         RunnerState#prstate.user#user.name,
                         Res]),
        {error, Res} 
    end.

%% @hidden 
%%  Links a list of PIDs to the local process. We use this when the Module:init/1
%%  function returns a list of PIDs (of spawned sub processes).
linker([])-> ok;
linker([Pid|Rest]) -> linker(Pid), linker(Rest);
linker(Pid) when is_pid(Pid)-> link(Pid);%TODO: this maybe should just be a monitor...
linker(_) -> ok. %Generic pass, we ignore unknown init returns.

%% @hidden 
%%  Utility function for merging the incoming parameter list for a command, 
%%  and the default parameter list registered by the 
%%  PluginModule:register_cmds/0 function on installation. This takes into
%%  account only EMP_TYPES and requires that parameter types are valid upon
%%  installation.
%%  For example: AddTimer( Seconds, Minutes, Hours ) and I apply: AddTimer 10.
%%  It should merge the defaults for the Minutes and Hours parameter and call
%%  AddTimer( 10, 0, 0 ).
-spec merge_params( [ 'EMPPARAM'() ], [any()] ) -> 
                                {ok, [any()]} | {error, Reason :: binary()}.
merge_params([],_) -> 
    {ok,[]};
merge_params(Defaults, []) ->
    F = fun(DParam)-> DParam#param.default end,
    {ok, list:map(F,Defaults)};
merge_params(D,P) when is_list(D) and is_list(P) -> merge_params(D,P,[]).
merge_params([],_P,A) -> {ok,A};
merge_params(D,[],A) -> 
    F = fun(DParam)-> DParam#param.default end,
    {ok, A++lists:map(F,D)};
merge_params([Def|Rest], [P|PRest], A) ->
    case Def#param.type of
        ?EMP_IGNORE -> merge_params(Rest, PRest, A);
        ?EMP_STRING -> merge_params(Rest, PRest, A++[P]);
        ?EMP_NUMBER ->
            try
                {ok,Term,_} = erl_scan:string(binary_to_list(P)),
                case Term of
                    [{float,_,Num}]   -> merge_params(Rest,PRest,A++[Num]);
                    [{integer,_,Num}] -> merge_params(Rest,PRest,A++[Num]);
                    _ -> 
                        {error, 
                            list_to_binary(
                                io_lib:format("Arg ~p should be a number",
                                      [length(A)+1]))}
                end
            catch _:Res -> {error, Res} end;
        ?EMP_BOOLEAN -> merge_params(Rest,PRest,A++[to_bool(P)]);
        ?EMP_LIST(_SubType) -> %TODO: Test subtype of list.
            if is_list(P) -> %parsed from ejson, its almost always going to be a list.
                       merge_params(Rest,PRest,A++[P]);
               true ->
                try
                    {ok,Term,_} = erl_scan:string(binary_to_list(P)),
                    List = erl_parse:parse(Term),
                    if is_list(List) -> 
                           merge_params(Rest,PRest,A++[List]);
                       true -> 
                        {error, 
                         list_to_binary(
                            io_lib:format("Arg ~p should be a list",
                                          [length(A)+1]))}
                    end
                catch _:Res -> {error, Res} end
            end;
        ?EMP_DICT(_SubType) -> %TODO:its from ejson, so it will be {[ {k,v}, ... ]}
            emplog:warn("TYPE OF PARAMETER IS DICTIONARY, NOT IMPLEMENTED, SKIPPING."),
            merge_params(Rest,PRest,A);
        ?EMP_URL(URLType) -> 
            merge_params(Rest,PRest,A++[{url,URLType,P}]);
        ?EMP_FILE(Ext) ->
            merge_params(Rest,PRest,A++[{file,Ext,P}]);
        ?EMP_TYPESIG -> 
            try
                {ok,Term,_} = erl_scan:string(binary_to_list(P)),
                List = erl_parse:parse(Term),
                if is_list(List) -> %TODO: check elements are integers/strings or sub lists.
                        merge_params(Rest,PRest,A++[{typesig,List}]);
                   true -> 
                    {error, 
                     list_to_binary(
                        io_lib:format("Arg ~p should be a type signature",
                                      [length(A)+1]))}
                end
            catch _:Res -> {error, Res} end;
        ?EMP_UNIQUE(Module, Type) -> 
            try
                {ok,Term,_} = erl_scan:string(binary_to_list(P)),
                List = erl_parse:parse(Term),
                if is_list(List) -> %TODO: check elements are integers/strings or sub lists.
                        merge_params(Rest,PRest,A++[{unique,Module,Type,List}]);
                   true -> 
                    {error, 
                     list_to_binary(
                        io_lib:format("Arg ~p should be a type signature",
                                      [length(A)+1]))}
                end
            catch _:Res -> {error, Res} end;
        Unknown ->
            emplog:debug("Invalid type signature: ~p",[Unknown]),
            throw(io_lib:format("Invalid type signature: ~p",[Unknown]))
    end.

%% @hidden Utility function for turning binary strings into erlang boolean().
to_bool(<<"true">>)->true;
to_bool(<<"True">>)->true;
to_bool(<<"1">>)->true;
to_bool(<<"yes">>)->true;
to_bool(<<"Yes">>)->true;
to_bool(_)->false.