%% EProc.erl
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
%% @see eman. <em>Event Manager</em>
%% @see plugrunner. <em>Plug Runner</em>
%%
%% @doc 
%%  The Event Processor facilitates the triggering of commands based on event
%%  subscriptions. One EProc is spawned for every plugin that a user is 
%%  running and once spawned the EProc is linked to the plugin's plugrunner. 
%%  All EProcs for a given user are within a single process group managed by
%%  the Event Manager. This is how it is able to broadcast every event.
%%  <br/><br/>
%%  Each EProc maintains the list of subscriptions whos trigger is for the 
%%  plugin it is linked to. By this we mean, if a timer plugin triggers the
%%  TimerEnded event and that is subscribed to the executor plugin's command 
%%  'run(&lt;&lt;"firefox"&gt;&gt;)', the EProc linked to the executor plugin
%%  will be the one facilitating the function calling. NOT the timer plugin.
%% @end
%% ----------------------------------------------------------------------------
-module(eproc).
-behaviour(gen_server).
-include("empinternal.hrl").

-record(epstate,
        {
            user          :: 'EMPUSER'(),
            plugin        :: 'EMPPLUGINDEF'(),
            subscriptions :: [ 'EMPSUBSCRIPTION'() ]
        }).

-define(REG_NAME(User,Plugin), 
            erlang:binary_to_atom(
                erlang:iolist_to_binary(
                    [<<"ep_">>,
                     User#user.name,
                     <<"_">>,
                     erlang:atom_to_binary(Plugin#plugindef.module,latin1)
                    ]), latin1)).

% Starting/Stopping server.
-export([start/1, start_link/1, stop/2]).
% `gen_server' callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%
%%% ---------------------------------------------------------------------------
%%% Server API
%%% ---------------------------------------------------------------------------
%%%

%% @doc Start and link a eproc instance for a particular user and plugin.
start( Args={User, _PRPid, Plugin, _Subscriptions} ) ->
    X = ?REG_NAME(User,Plugin),
    gen_server:start({local,X}, ?MODULE, Args, []).

%% @doc Start and link a eproc instance for a particular user and plugin.
start_link( Args={User, _PRPid, Plugin, _Subscriptions} ) ->
    X = ?REG_NAME(User,Plugin),
    gen_server:start_link({local,X}, ?MODULE, Args, []).

%% @doc 
%%  Stops a running EProc. <br/> <em>Warning:</em> don't shut them down by 
%%  hand, it wont bring down the plugrunner and therefore your subscriptions 
%%  wont ever trigger, until you by hand bring up and down the plugin.
%% @end
-spec stop( 'EMPUSER'(), 'EMPPLUGINDEF'() ) -> ok.
stop(User, Plugin) ->
    X = ?REG_NAME(User,Plugin),
    gen_server:cast(X, {internal, shutdown, Plugin}).


%%%
%%% ----------------------------------------------------------------------------
%%% Callback functions for `gen_server`.
%%% ----------------------------------------------------------------------------
%%%

%% @private
%% @doc `gen_server' callback from start and start_link.
init({User,PRPid,Plugin,Subscriptions}) ->
    process_flag(trap_exit, true),
    link(PRPid),
    {ok, #epstate{user=User, plugin=Plugin, subscriptions=Subscriptions}}.

%% @private
%% @doc
%%  We shouldn't get calls, as all messages should be either coming from 
%%  process groups (handle_info), or link breaks with the pluginrunner.
%% @end
handle_call(Msg,_,State)->
    emplog:warn("eproc:call-Called message unexpected for Plugin(~p): ~p",
                    [State#epstate.plugin#plugindef.module, Msg]).

%% @private
%% @doc The only cast is for shutting down.
handle_cast({internal,shutdown,_Plugin},State)->
    {stop, normal, State};
handle_cast(Msg,State)->
    emplog:warn(
      io_lib:format("eproc:cast-Casted message unexpected for Plugin(~p): ~p",
                    [State#epstate.plugin#plugindef.module, Msg])).   

%% @private
%% @doc
%% `gen_server' callback. Accepts messaged from the process group and from the 
%% link it has with its plugrunner.
%% @end
handle_info( {'EXIT',From,Reason}, State) -> 
    emplog:debug("eproc got exit message from ~p(~p), stopping.",[From,Reason]),
    {stop, normal, State};

% Messages from the processes group.
handle_info( {pg_message, _, _, {shutdown, Module}}, State ) -> 
    if Module==State#epstate.plugin#plugindef.module -> 
           {stop, State};
       true -> 
           {noreply, State}
    end;
handle_info( {pg_message, _, _, {add_sub, Module, Sub}}, State ) -> 
    if Module==State#epstate.plugin#plugindef.module ->
           Subs = State#epstate.subscriptions,
           {noreply, State#epstate{subscriptions=[Sub|Subs]}};
       true -> 
           {noreply, State}
    end;
handle_info({pg_message,_,_,{event,Event}}, State) ->
    spawn( fun() -> handle_event(State, Event) end ),
    {noreply, State};
handle_info({new_member,_Name,_Pid}, State) -> {noreply, State}; 
handle_info({crashed_member, _Name, _Pid}, State) -> {noreply, State};

% Unknown messages
handle_info(Msg, State) ->
    emplog:warn("eproc:info-Message unexpected for Plugin(~p): ~p",
                    [State#epstate.plugin#plugindef.module, Msg]),
    {noreply, State}.

%% @private
%% `gen_server` callback. Unused.
terminate({error, Reason}, _State) -> 
    emplog:error("eproc terminated with an error:", [Reason] ),
    shutdown;
terminate(_Reason, _State)-> shutdown.

%% @private
%% `gen_server` callback. Unused.
code_change(_OldVsn, State, _Extra)-> {ok, State}.


%%%
%%% ---------------------------------------------------------------------------
%%% Private functions
%%% ---------------------------------------------------------------------------
%%%

%% @hidden Actually handles the event trigger.
-spec handle_event( #epstate{}, { atom(), binary(), [any()]} ) -> ok. 
handle_event(State, {Plugin, EventName, ValueList}) ->
    emplog:debug("Eproc(~p,~p) got event ~p",[State#epstate.plugin#plugindef.module,State#epstate.user#user.name,EventName]),
    if State#epstate.plugin#plugindef.module =:= Plugin ->
           %TODO: Handle the event!  
            emplog:event(io_lib:format("[~p]~p:~p",[Plugin,EventName,ValueList]));
       true -> ok
    end.
