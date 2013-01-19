%% EMPD_Sup.erl
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
%% @see iman. <em>Interface Manager</em>
%% @see uwatcher. <em>User Tree Manager</em>
%% @see gman. <em>Message Governer</em>
%%
%% @doc 
%%   This is the EMP Service Monitor. It is the top level process that
%%   watches over the three primary processes that make up the EMP server.
%%   These are the interface server, the user tree manager, and the message
%%   governor.
%% @end
%% ----------------------------------------------------------------------------
-module(empd_sup).
-behaviour(supervisor).

%% Supervisor API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Fun, Args), {I, {I, Fun, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Start the emp core server.
start_link( _StartArgs_ ) ->
    process_flag(trap_exit, true),
    emplog:debug("empd starting..."),
    supervisor:start_link({global, empd}, ?MODULE, []).
    

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private
%% @doc `gen_server' callback from start_link.
init([]) ->
    {ok, { {one_for_one, 5, 10}, 
            % Processes that EMPD launches and keeps track of.
            % interface manager, user manager, and Gman.
            [ ?CHILD(gman, worker, start_link, []),
              ?CHILD(uwatcher, supervisor, start_link, []),
              ?CHILD(iman, supervisor, start_link, [])
            ] } }.
