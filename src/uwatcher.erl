%% UWatcher.erl
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
%% @see uman_serve. <em>User manager</em>
%%
%% @doc 
%%  User watcher is a supervisor that generates and runs all user nodes
%%  as their own processes. This will monitor if any of the user's crash,
%%  if this happens then it will restart them. This is to help improve 
%%  individual reliability of the user's data/events as well as protect 
%%  other users from downtime another user may cause.
%% @end
%% ----------------------------------------------------------------------------
-module(uwatcher).
-behaviour(supervisor).
-include("empinternal.hrl").

%% Supervisor API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(U, I, Type, Fun, Args), 
            {U, {I, Fun, Args}, permanent, 5000, Type, [I]}).

%% Process registration relys on the user's name.
-define(REG_NAME(User), ?PROCESS_NAME( User#user.name, "_uman" )). 


%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Starts the User Watcher.
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
   %process_flag(trap_exit, true), %XXX: commented out for testing purposes.
    supervisor:start_link({local,?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private
%% @doc `supervisor' callback from start_link.
init( [] ) ->
    Users = empdb:get_users(),
    Root = empdb:get_root_user(),
    {ok,{{one_for_one, 5, 10},
            [
             % Must start root first.
             ?CHILD(root_uman, uman_sup, supervisor, start_link, [Root])
             | generate_usernodes( Users )
            ]
        }
    }.


%% ===================================================================
%% Private functions
%% ===================================================================

%% @hidden Utility function for creating the init list for the rest of 
%%  the user nodes.
generate_usernodes(L)-> generate_usernodes(L,[]).
generate_usernodes([],A) -> A;
generate_usernodes([User | Users], A) ->
    UniqueName = ?REG_NAME(User),
    Child = ?CHILD(UniqueName, uman_sup, supervisor, start_link, [User]),
    NewA = [Child|A],
    generate_usernodes(Users,NewA).

