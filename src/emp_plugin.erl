%% Emp_Plugin.erl 
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
%% @see plugrunner. <em>Plugin Runner</em>
%%
%% @doc 
%%  This module defines a host of functions that EMP plugins can use to 
%%  access and set state, trigger events and generally talk to the core of
%%  EMP. This module is also an erlang behaviour, that specifies several
%%  callbacks that are required by the EMP Plugin API; these are:<br/>
%%
%%  <ul>
%%    <li><pre> registration_info() -> [ {atom(), any()} ].</pre>
%%      EMP requires, upon plugin installation/registration, that the plugin
%%      informs EMP of the permissions that it will need, in order to function
%%      correctly. For a list of these permissions, and all options that can
%%      be specified, please check the PluginAPI documentation.
%%    </li>
%%
%%    <li><pre> register_cmds() -> [ 'EMPCOMMAND'() ].</pre>
%%      When a plugin is registered, it needs to give a complete list of all
%%      commands that can be run on it. Please see emptypes.hrl for more 
%%      information on how to construct an EMPCOMMAND.
%%    </li>
%%
%%    <li><pre> register_events() -> [ 'EMPEVENT'() ].</pre>
%%      When a plugin is registered, it needs to give a complete list of all
%%      events that can be triggered. That is, all types of events possible, 
%%      not the event instances themselves (i.e., TimerEnded is an event type, 
%%      and Timer #2315123 just ending is an instance of the event TimerEnded.)
%%      Please see emptypes.hrl for more information on how to construct an 
%%      EMPEVENT.
%%    </li>
%%  </ul>
%%
%% @end
%% ----------------------------------------------------------------------------
-module(emp_plugin).
-include("emptypes.hrl").

%% Public API for EMP, gauranteed to each plugin.
-export([ set_var/3, set_var_async/3, get_var/2, update_var/3 ]). 
-export([ trigger_event/3, trigger_event/2, request_shutdown/2 ]).

-callback registration_info() -> [ {atom(), any()} ]. % required
-callback register_cmds()     -> [ 'EMPCOMMAND'() ].  % required
-callback register_events()   -> [ 'EMPEVENT'() ].    % required
% callback init( 'STATEID'() ) -> ok | {ok, [pid()]} | {error, binary()}. % optional
% callback finish( 'STATEID'() ) -> ok | {error, binary()}                % optional

%%
%% ===========================================================================
%% Public API implementation.
%% ===========================================================================
%%

%% @doc
%% Trigger an event that your plugin has registered previously. If it doesn't
%% exist, these functions will fail silently. If no override values are provided, 
%% the event will be triggered with default parameters.
%% @end
-spec trigger_event( 'STATEID'(), binary() ) -> ok.
trigger_event( StatePid, EventName ) ->
    StatePid ! {plugin, event, EventName, []}, ok.

%% @doc
%% Trigger an event that your plugin has registered previously. If it doesn't
%% exist, these functions will fail silently. 
%% @end
-spec trigger_event( 'STATEID'(), binary(), [ {binary(), any()} ] ) -> ok.
trigger_event( StatePid, EventName, ValueList ) ->
    StatePid ! {plugin, event, EventName, ValueList}, ok.

%% @doc
%% Request that the plugin be shutdown. If its not shutdown, then it could be
%% restarted next time a user or event trigger a command to be run on the 
%% module. A use for this is if the plugin shouldn't be run (i.e. a missing
%% library).
%% @end
-spec request_shutdown( 'STATEID'(), binary() ) -> ok.
request_shutdown( StatePid, Reason ) ->
    StatePid ! {plugin, shutdown, Reason}, ok.

%% @doc Get some variable you saved in EMP.
-spec get_var( 'STATEID'(), binary() ) -> {ok, any()} | {error, binary()}.
get_var(StatePid, VarName) ->
    StatePid ! {plugin, get_var, self(), VarName},
    receive Res -> Res end.

%% @doc 
%% Set some variable you want to save in EMP. This will override the 
%% current value in EMP if its present, otherwise it will add it.
%% @end
-spec set_var( 'STATEID'(), binary(), X ) -> {ok, X} | {error, binary()}.
set_var(StatePid, VarName, NewVal) ->
    StatePid ! {plugin, set_var, self(), VarName, NewVal},
    receive Res -> Res end.

%% @doc
%% Does not wait on EMP to finish saving your state. This can be dangerous
%% if your plugin is using unsaved state, as any commands triggered after that 
%% point wont have that new data.
%% @end
-spec set_var_async( 'STATEID'(), binary(), X ) -> {ok, X}.
set_var_async(StatePid, VarName, NewVal) ->
    StatePid ! {plugin, set_var, async, self(), VarName, NewVal}, 
    {ok, NewVal}.

%% @deprecated <em>WARNING:</em> THIS IS AN ERLANG ONLY FUNCTION CALL.<br/>
%% @doc 
%% Runs a function on the EMP server to update a state variable. Useful 
%% for running async foreach loops on a variable that is a list. 
%% @end
-spec update_var( 'STATEID'(), binary(), fun() ) -> {ok, any()} | {error, binary()}.
update_var(StatePid, VarName, ApplyFun) ->
    StatePid ! {plugin, update_var, self(), VarName, ApplyFun},
    receive Res -> Res end.

