%% UMan_Serve.erl
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
%% @doc 
%%  The User Management Server is the primary source of Message handling for a 
%%  given user within EMP. If a user thinks they are talking to EMP they are 
%%  actually talking to their specific UMan server. This server also maintains
%%  the user's state and keeps communication open to their individual plugins.
%%  <br/><br/>
%%  A User Management Server is self contained and if there is ever an error
%%  with a plugin and it crashes, the user will still be able to run commands
%%  and queries with EMP. Even if a particular query fails, the UMan won't
%%  go down as all queries are spawned to new processes.
%% @end
%% ----------------------------------------------------------------------------
-module( uman_serve ).
-behavior( gen_server ).
-include( "empinternal.hrl" ).
-include("uman_state.hrl").

%% gen_server API
-export([start_link/1, stop/1]).

%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% uman API
-export([connect_plugin/3, disconnect_plugin/2]).

%% gman API
-export([run_query/2, run_command/4, run_command_async/4]).

%% The globally registered uman for this user.
-define(REG_NAME(User), {global, ?PROCESS_NAME( User#user.name, "_uman" )}).
-define(REG_UNAME(UName), {global, ?PROCESS_NAME( UName, "_uman" )}).

%%%
%%% ======================================================================
%%% PLUGRUNNER API
%%% ======================================================================
%%%


%% @doc
%%  Connect the plugin to the UMan, and then register/link the plugin with 
%%  an EProc via EMan.
%% @end
-spec connect_plugin( 'EMPUSER'(), pid(), 'EMPPLUGINDEF'() ) -> ok.
connect_plugin( User, PRPid, PluginDef ) ->
    gen_server:cast(?REG_NAME(User), {add_plugin, {PluginDef, PRPid}} ),
    eman:link_eproc(User, PRPid, PluginDef).

%% @doc Remove the plugin when its in the process of shutting down.
-spec disconnect_plugin( 'EMPUSER'(), pid() ) -> ok.
disconnect_plugin( User, PRid ) ->
    gen_server:cast(?REG_NAME(User),{rm_plugin, PRid}).


%%%
%%% ======================================================================
%%% GMAN/EProc API
%%% ======================================================================
%%%

%% @doc Run a query on EMP or a plugin. This is called by interfaces.
-spec run_query( 'EMPUSER'(), binary() ) -> encodable_return().
run_query( User, Query ) ->
    emplog:debug("~p sent a query: ~p",[User#user.name, Query]),
    gen_server:call(?REG_NAME(User), {run_query, Query}).

%% @doc Runa a command on EMP or a plugin. This is called by interfaces or EProcs.
-spec run_command( 'EMPUSER'(), binary(), binary(), [any()] ) -> encodable_return().
run_command(User, Target, Command, ParamList) ->
    emplog:debug("~p sent command to ~p, ~p( ~p )",
                 [User#user.name, Target,Command,ParamList]),
    gen_server:call(?REG_NAME(User), {run_cmd, Target, Command, ParamList}).

%% @doc Runa a command on EMP or a plugin. This is called by interfaces or EProcs.
-spec run_command_async( 'EMPUSER'(), binary(), binary(), [any()] ) -> ok.
run_command_async(User, Target, Command, ParamList) ->
    emplog:debug("~p sent command to ~p, ~p( ~p )",
                 [User#user.name, Target,Command,ParamList]),
    gen_server:cast(?REG_UNAME(User), {run_cmd, Target, Command, ParamList}).


%%%
%%% ======================================================================
%%% Server API
%%% ======================================================================
%%%

%% @doc Start a User Manager for a particular user.
-spec start_link( 'EMPUSER'() ) -> {ok, pid()} | {error, Reason :: any()}.
start_link( User ) ->
    emplog:debug("Starting uman_serve for: ~p",[User#user.name]),
    gen_server:start_link(?REG_NAME(User), ?MODULE, [User], []).

%% @doc Stop a User Manager for a particular user.
-spec stop( 'EMPUSER'() ) -> ok.
stop( User ) ->
    gen_server:cast(?REG_NAME(User), {internal, shutdown}).


%%%
%%% ======================================================================
%%% Private callbacks.
%%% ======================================================================
%%%

%% @private
%% @doc `gen_server' callback from start_link.
init([User]) ->
    {ok, #ustate{user=User}}.

%% @private
%% @doc `gen_server' callback.
handle_call({run_cmd, null, Command, Params}, From, State) ->
    {noreply, uman_cmd:handle_cmd( Command, Params, From, State )};
handle_call({run_cmd, <<"emp">>, Command, Params}, From, State) ->
    {noreply, uman_cmd:handle_cmd( Command, Params, From, State )};
handle_call({run_cmd, PluginName, Command, Params}, From, State) ->
    case lists:keyfind(PluginName, 1, State#ustate.targets) of
        false -> %If no such Plugin by that name, return error.
            Err = list_to_binary(io_lib:format("Invalid Target: ~p",[PluginName])),
            {reply, {error,Err}, State};
        {PluginName, PluginProcess} ->
            case lists:keyfind(PluginProcess, 1, State#ustate.pluginrunners) of
                false -> % If plugin is known, but not running, return error.
                    {reply, {error,<<"Plugin isn't running">>}, State};
                {PluginProcess, PRunner} ->
                    spawn( uman_cmd, handle_plugin_cmd,[PRunner, Command,Params, From] ),
                    {noreply, State}
            end
    end;
handle_call({run_query, Query}, From, State) ->
    spawn( uman_cmd, handle_query, [Query, From, State] ),
    {noreply, State};
handle_call(_Request, _From, State)-> 
    {reply, <<"NOT IMPLEMENTED">>, State}.

%% @private
%% @doc `gen_server' callback.
handle_cast( {add_plugin, PlugPair}, State ) ->
    {PDef, Pid} = PlugPair,
    User = State#ustate.user,
    Plugins = State#ustate.pluginrunners,
    Targets = State#ustate.targets,
    NewState = State#ustate{
                   pluginrunners=[{PDef,Pid}|Plugins],
                   targets=[{empdb:get_target(User, PDef),PDef}|Targets]
                           },
    {noreply, NewState};
handle_cast( {rm_plugin, PlugPid}, State ) ->
    case lists:keytake(PlugPid, 2, State#ustate.pluginrunners) of
        {value, {Def,_}, NewPlugs} -> 
            NewVals = lists:keydelete(Def, 2, State#ustate.targets),
            NewState = State#ustate{pluginrunners=NewPlugs, targets=NewVals},
            {noreply, NewState};
        _ -> {noreply, State}
    end;
handle_cast( {run_cmd, Target, Command, Params}, State) ->
    case Target of
        % target is emp itself, but each user's server state is different 
        % so uman handles it.
        null  -> 
            {_,NewState}=uman_cmd:handle_cmd( Command, Params, State ),
            {noreply, NewState};
        "emp" -> 
            {_,NewState}=uman_cmd:handle_cmd( Command, Params, State ),
            {noreply, NewState};
        
        % target is a plugin, so lets verify its a valid plugin, and run it
        % separately. We can thread this as our state wont change.
        _ ->
            case lists:keyfind(Target, 1, State#ustate.targets) of
                false  -> {noreply, State};
                Plugin ->
                    spawn( uman_cmd,
                       handle_plugin_cmd,
                           [Plugin, Command, Params] ),
                    {noreply, State}
            end
    end;
handle_cast({internal, shutdown}, State)->
    {stop, normal, State};
handle_cast( Request, State )-> 
    emplog:warn("uman:cast unknown request ~p",[Request]),
    {noreply, State}. 

%% @private
%% @doc `gen_server' callback. Unused.
handle_info(_Info, State)-> {noreply, State}.

%% @private
%% @doc `gen_server' callback. Unused.
terminate({error, Reason}, _State) ->
    emplog:error("uman_serve:terminate with error", [Reason]),
    shutdown;
terminate(_Reason, _State)-> shutdown.

%% @private
%% @doc `gen_server' callback. Unused.
code_change(_OldVsn, State, _Extra)-> {ok, State}.
