%% SEProc.erl
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
%% @see gman. <em>Message Governer</em>
%% @see eman. <em>User Event Manager</em>
%%
%% @doc 
%%  When EMP starts up, the root eman creates N System Plugin Event Processors 
%%  (seproc), where N is the number of users. Each seproc is responsible for 
%%  sending its user's eman an event for every one it recieves where there is 
%%  a user subscription for.
%% @end
%% ----------------------------------------------------------------------------
-module(seproc).
-include("empinternal.hrl").

-record(sepstate,
        {
            user          :: 'EMPUSER'(),
            emanref       :: tuple()
        }).

-define(REG_NAME(User), 
        erlang:binary_to_atom(
                erlang:iolist_to_binary(
                    [<<"sep_">>, User#user.name]))).

% Starting/Stopping server.
-export([start/2, start_link/2, stop/1]).
% `gen_server' callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%
%%% ---------------------------------------------------------------------------
%%% Server API
%%% ---------------------------------------------------------------------------
%%%

%% @doc Start and link a ueproc instance for a particular user.
-spec start( 'EMPUSER'(), tuple() ) -> 
          ok | {error, any()}.
start( User, Eman ) ->
    X=?REG_NAME(User),
    gen_server:start({local,X}, ?MODULE, [User, Eman], []).

%% @doc Start and link a ueproc instance for a particular user.
-spec start_link( 'EMPUSER'(), tuple() ) ->
          {ok, pid()} | {error, any()}.
start_link( User, Eman ) ->
    X=?REG_NAME(User),
    gen_server:start_link({local,X}, ?MODULE, [User, Eman], []).

%% @doc Stops a running UEProc. 
-spec stop( 'EMPUSER'() ) -> ok.
stop(User) ->
    X=?REG_NAME(User),
    gen_server:cast(X, {internal, shutdown}).


%%%
%%% ----------------------------------------------------------------------------
%%% Callback functions for `gen_server`.
%%% ----------------------------------------------------------------------------
%%%

%% @private
%% @doc `gen_server' callback from start and start_link.
init([User,EMan]) ->
    process_flag(trap_exit, true),
    {ok, #sepstate{user=User, emanref=EMan}}.

%% @private
%% @doc
%%  We shouldn't get calls, as all messages should be either coming from 
%%  process groups (handle_info), or link breaks with the EMan.
%% @end
handle_call(Msg,_,State)->
    emplog:warn("ueproc:call-Called message unexpected for User(~p): ~p",
                    [State#sepstate.user#user.name, Msg]).

%% @private
%% @doc The only cast is for shutting down.
handle_cast({internal,shutdown},State)->
    {stop, normal, State};
handle_cast(Msg,State)->
    emplog:warn(
      io_lib:format("eproc:cast-Casted message unexpected for User(~p): ~p",
                    [State#sepstate.user#user.name, Msg])).   

%% @private
%% @doc
%% `gen_server' callback. Accepts messaged from the process group and from the 
%% link it has with its EMan.
%% @end
handle_info( {'EXIT',From,Reason}, State) -> 
    emplog:debug("seproc got exit message from ~p(~p), stopping.",[From,Reason]),
    {stop, normal, State};

% Messages from the processes group.
handle_info( {pg_message, _, _, {shutdown, User}}, State ) -> 
    if User==State#sepstate.user#user.name -> 
           {stop, State};
       true -> 
           {noreply, State}
    end;
handle_info({pg_message,_,_,{event,Event}}, State) ->
    eman:trigger_sysevent( State#sepstate.emanref, Event ),
    {noreply, State};
handle_info({new_member,_Name,_Pid}, State) -> {noreply, State}; 
handle_info({crashed_member, _Name, _Pid}, State) -> {noreply, State};

% Unknown messages
handle_info(Msg, State) ->
    emplog:warn("seproc:info-Message unexpected for User(~p): ~p",
                    [State#sepstate.user#user.name, Msg]),
    {noreply, State}.

%% @private
%% `gen_server` callback. Unused.
terminate({error, Reason}, _State) -> 
    emplog:error("seproc terminated with an error:", [Reason] ),
    shutdown;
terminate(_Reason, _State)-> shutdown.

%% @private
%% `gen_server` callback. Unused.
code_change(_OldVsn, State, _Extra)-> {ok, State}.


