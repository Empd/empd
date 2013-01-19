%% PlugRunner.erl
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
%% @see emp_plugin. <em>EMP Plugin Behaviour</em>
%% @see pman. <em>Plugin Manager</em>
%%
%% @doc 
%%  Plugin runner is a server that communicates with a plugin on behalf 
%%  of Emp. It has functions for the running of commands and setting
%%  the variables of a given plugin. This makes it easy for uman to
%%  talk to plugins.
%% @end
%% ----------------------------------------------------------------------------
-module(plugrunner).
-include("empinternal.hrl").
-include("pr_state.hrl").

% Public/Server exportss
-export([start_link/2]).
% Private exports
-export([start_loop/1]).


%%%
%%% ==========================================================================
%%% Public Server Functions
%%% ==========================================================================
%%%

%% @doc Starts and links the plugin runner's monitor process.
-spec start_link( 'EMPUSER'(), 'EMPPLUGINDEF'() ) -> {ok, pid()} | {error, Reason :: atom()}.
start_link( User, PluginDefinition ) ->
    Module = PluginDefinition#plugindef.module,
    State = #prstate{user=User, 
                     plugin_id=PluginDefinition#plugindef.id,
                     plugin_module=Module,
                     plugin_state=PluginDefinition#plugindef.state,
                     commands=PluginDefinition#plugindef.commands},
    emplog:debug("Starting pluginrunner for ~p on user ~p",[Module,User#user.name]),
    spawn_loop(State, Module, User, PluginDefinition).


%%%
%%% ==========================================================================
%%% Private Functions
%%% ==========================================================================
%%%

%% @private
%% @doc Traps all exit events so nothing rebounds into pman, and starts the loop.
-spec start_loop( #prstate{} ) -> normal.
start_loop( State ) ->
    process_flag(trap_exit, true), 
    server_loop(State).


%%%
%%% ==========================================================================
%%% Internal PlugRunner functionality.
%%% ==========================================================================
%%%


%% @hidden Spawns the monitor, and initializes the plugin.
spawn_loop( State, Module, User, PlugDef ) -> % {ok, Pid} | {error, Reason}
    RunnerPid = spawn_link( plugrunner, start_loop, [State] ),
    case initialize_plugin(RunnerPid, Module) of
        ok ->
            uman_serve:connect_plugin(User, RunnerPid, PlugDef),
            {ok, RunnerPid};
        Err -> Err
    end.

%% @hidden Checks to see if the 'init' function is in the module, then runs it.
initialize_plugin( RunnerPid, Module ) -> % ok | {error, Reason}
    case code:ensure_loaded(Module) of
        {module, Module} -> 
            run_plugin_init(RunnerPid, Module);
        _ -> {error, badarg} %unknown module
    end.

%% @hidden Handles break down steps before completly shutting down the plugrunner.
shutdown_module( SuperPid, State ) -> % ok
    Module = State#prstate.plugin_module,
    emplog:debug("Stopping pluginrunner for ~p on user ~p",
                      [Module,State#prstate.user#user.name]),
    run_plugin_finish( SuperPid, Module ),
    SuperPid ! {internal, final_shutdown},
    ok.

%% @hidden The message handler loop.
server_loop(State) -> % normal
    StateChange = receive Message -> pr_msg_handler:handle_message( State, Message ) end,
    case StateChange of
        no_change -> server_loop(State);
        shutdown  -> 
            SuperPid = self(),
            spawn(fun()-> shutdown_module(SuperPid, State) end), %calls this process to inform of finish.
            server_loop(State); %keep plugrunner open so finish method can have state access.
        final_shutdown -> 
            uman_serve:disconnect_plugin(State#prstate.user, self()),
            normal; %exit with normal response
        NewState  -> server_loop(NewState)
    end.

%% @hidden Utility function for checking the existence of and then running the init/1 function.
run_plugin_init( RunnerPid, Module ) -> % {ok, RunnerPid} | {error, Reason}
    case lists:keyfind(init, 1, Module:module_info(exports)) of
        {init,1} ->
            case apply(Module, init, [RunnerPid]) of
                ok -> 
                    ok;
                {ok, PidList} ->
                    RunnerPid ! {internal, link_pids, PidList};
                {error, Reason} -> 
                    emplog:warn("Init Plugin Failure (~p): ~p",[Module, Reason]),
                    {error, Reason};
                UnknownReturnValue ->
                    emplog:warn("Unknown return message from ~p:init = ~p",[Module,UnknownReturnValue])
            end;
        {init,_BadArity} -> 
            emplog:debug("Plugin( ~p ) has a bad number of arguments for its Init function!",[Module]),
            ok;
        _ -> 
            ok % doesn't have init/1, its optional so we don't care.
    end.

%% @hidden Utility function for checking the existence of and then running the finish/1 function.
run_plugin_finish( RunnerPid, Module ) -> % ok
  case lists:keyfind(finish, 1, Module:module_info(exports)) of
        {finish,1} ->
            case apply(Module, finish, [RunnerPid]) of
                ok -> ok;
                {error, Reason} -> 
                    emplog:warn("Finish Plugin Failure (~p): ~p",[Module, Reason]);
                UnknownReturnValue ->
                    emplog:warn("Unknown return message from ~p:finish = ~p",[Module,UnknownReturnValue])
            end;
        {finish,_BadArity} -> 
            emplog:debug("Plugin( ~p ) has a bad number of arguments for its finish function!",[Module]);
        _ -> 
            ok % doesn't have finish/0, its optional so we don't care.
  end, ok.
