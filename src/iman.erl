%% IMan.erl 
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
%% @see empinterface. <em>EMP Interface API Module</em>
%%
%% @doc 
%%  The Interface manager is a supervisor for all the interface handlers.
%%  Each handler can monitor a different type of connection protocol, so
%%  we can have a TCPHandler, UDPHandler or even your own internal 
%%  protocol you define to talk to EMP. If ever any of them go down, they
%%  will get restarted.
%% @end
%% ----------------------------------------------------------------------------
-module( iman ).
-behaviour( supervisor ).

%% Supervisor API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define( CHILD(I, Args), 
         {I, {I, start, [normal, Args]}, permanent, 5000, supervisor, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Start the IMan server and all EMP interfaces.
start_link() ->
    process_flag(trap_exit, true),
    emplog:debug("interface manager starting..."),
    supervisor:start_link({local,?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private
%% @doc Grab all interfaces from empd environment variable 'ifaces'.
init( [] ) ->
    {ok, Ifaces} = application:get_env(empd, ifaces),
    Children = lists:foldl( fun load_iface/2, [], Ifaces ),
    {ok,{{one_for_one, 5, 10}, Children}}.

%% @hidden Creates an init child-list using the `ifaces` env. variable.
load_iface( {Mod, Args}, Acc ) ->
    [ ?CHILD(Mod, Args ) | Acc ].
