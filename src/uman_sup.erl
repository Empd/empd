%% UMan_Sup.erl
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
%%  The supervisor portion of the UMAN logical block. It launches the uman's 
%%  gen_server and the user's pman and eman. All connections done between eman
%%  and pman are done between their APIs. This just monitors their uptime and
%%  will respond to fatal errors only.
%% @end
%% ----------------------------------------------------------------------------
-module( uman_sup ).
-behavior( supervisor ).
-include("empinternal.hrl").

%% Supervisor API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Fun, Args), {I, {I, Fun, Args}, permanent, 5000, Type, [I]}).

%% The registration of the user
-define(REG_NAME(User), ?PROCESS_NAME( User#user.name, "_sup" )).

%%%
%%% ===================================================================
%%% API functions
%%% ===================================================================
%%%

%% @doc Start the User Manager Supervisor.
-spec start_link( 'EMPUSER'() ) -> {ok, pid()} | {error, any()}.
start_link( User ) ->
    process_flag(trap_exit, true), %XXX: Commented out for testing!
    supervisor:start_link({global, ?REG_NAME(User)}, ?MODULE, [User]).


%%%
%%% ===================================================================
%%% Supervisor callbacks
%%% ===================================================================
%%%

%% @private
%% @doc `gen_server' callback for start_link.
init([User]) ->
    {ok, { {one_for_all, 5, 10}, 
            [
            ?CHILD(uman_serve, worker, start_link, [User]),
            ?CHILD(eman, worker, start_link, [User]),
            ?CHILD(pman_sup, worker, start_link, [User])
            ] }}.