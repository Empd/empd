%% PMan_Sup.erl
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
%% @see plugrunner. <em>Plug Runner</em>
%%
%% @doc 
%%  The PMan (Plugin Manager) is a supervisor that keeps track of the plugins 
%%  and will monitor for if they crash, it will restart them if this happens.
%% @end
%% ----------------------------------------------------------------------------
-module( pman_sup ).
-behaviour( supervisor ).
-include("empinternal.hrl"). 

%% Supervisor API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% The Name of the supervisor that globaly registered.
-define(REG_NAME(User), {global, ?PROCESS_NAME(User#user.name,"_pman")}).

%% Helper macro for declaring children of supervisor
-define(CHILD(U,I, Type, Fun, Args), {U, {I, Fun, Args}, transient, 5000, Type, [I]}).

%%%
%%% ===================================================================
%%% API functions
%%% ===================================================================
%%%

%% @doc Starts the Plugin Manager.
-spec start_link( 'EMPUSER'() ) -> {ok, pid()} | {error, any()}.
start_link( User ) ->
    process_flag(trap_exit, true),
    supervisor:start_link(?REG_NAME(User), ?MODULE, [User]).


%%%
%%% ===================================================================
%%% Supervisor callbacks
%%% ===================================================================
%%%

%% @private
%% @doc `supervisor' callback from start_link.
init( [User] ) ->
    {ok, { {one_for_one, 5, 10}, load_plugrunners(User) } }.


%%%
%%% ===================================================================
%%% Internal Functionality
%%% ===================================================================
%%%

%% @hidden Pulls the list of installed plugin definitions from the database.
load_plugrunners(User) ->
    case empdb:get_plugin_defs( User#user.uid ) of
        {error, Reason} ->
            emplog:debug("Error grabbing plugin definitions from database: ~p", [Reason]),
            [];
        PluginDefinitions ->
            make_pluginrunners(User, PluginDefinitions, [])
    end.

%% @hidden Creates our supervisor list that init/1 returns.
make_pluginrunners(_User, [], A) -> A;
make_pluginrunners(User, [PluginDef|Rest],A)->
    Child = ?CHILD(?UUID(),
                   plugrunner,worker,start_link,
                   [User, PluginDef]),
    NA = [Child|A],
    make_pluginrunners(User,Rest,NA).

