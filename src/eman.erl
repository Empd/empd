%% EMan.erl
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
%% @see eproc. <em>Event Processor</em>
%% @see ueproc. <em>User Event Processor</em>
%%
%% @doc 
%%  The Event Manager handles the broadcasting of event messages to all 
%% handlers. It also is responsible for spawning handlers for all plugins so
%% that subscriptions can be handled effectively.
%%
%%  Please note that there is always just one Event Processor thread (eproc) 
%% per plugin. However, when it's the Root user's EMan, N System Plugin Event
%% Processors (seproc) are also created, where N is the number of Users.
%% @end
%% ----------------------------------------------------------------------------
-module(eman).
-behavior( gen_server ).
-include("empinternal.hrl").

% State of the eman gen_server
-record(estate, 
        {
            user          :: 'EMPUSER'(),
            is_root=false :: boolean()
        }).

% Root eman registration reference.
-define(ROOT_NAME, {global, root_eman}).
% Macros for creating registration references for a given 'EMPUSER'().
-define(P(U,S),?PROCESS_NAME(U#user.name, S)).
-define(REG_NAME(User), {global, ?P(User, "_eman")}).
-define(PROJ_GROUP(User),  ?P(User,"_eprocg")).
-define(SEPROJ_GROUP,seprocg).

% Starting/Stoping Eman Servers
-export([start_link/1, stop/1]).
% PLUGIN API exports.
-export([trigger_event/4, trigger_sysevent/2]).
% UMAN API exports.
-export([link_eproc/3,link_new_user/1]).
% Private `gen_server` callbacks.
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).


%%%
%%% --------------------------------------------------------------------------- 
%%% PLUGIN API 
%%% ---------------------------------------------------------------------------
%%%

%% @doc Called from emp_plugin, this will send the event to all plugins and gman.
-spec trigger_event( 'EMPUSER'(), atom(), string(), [any()] ) -> ok.
trigger_event(User, Plugin, EventName, ValueList)->
    gen_server:cast( ?REG_NAME(User),  {event, {Plugin, EventName, ValueList}}).


-spec trigger_sysevent( 'EMPUSER'(), tuple() ) -> ok.
trigger_sysevent(Eman, Event) ->
    gen_server:cast( Eman, {sysevent, Event}).

%%%
%%% --------------------------------------------------------------------------- 
%%% UMAN API 
%%% ---------------------------------------------------------------------------
%%%

%% @doc 
%%  Called when a plugin is started for a given user. This will ask the 
%%  user's eman to make an eproc for it.
%% @end  
-spec link_eproc( 'EMPUSER'(), pid(), 'EMPPLUGINDEF'() ) -> ok.
link_eproc(User, PRPid, PluginModule) ->
    gen_server:cast(?REG_NAME(User), {internal, make_eproc, PRPid, PluginModule}).

%% @doc 
%%  Called when new users are started at runtime.
%% @end
-spec link_new_user( 'EMPUSER'() ) -> ok.
link_new_user( User ) -> 
    gen_server:cast(?ROOT_NAME, {internal, make_seproc, User}).


%%%
%%% --------------------------------------------------------------------------- 
%%% PUBLIC `gen_server` API 
%%% ---------------------------------------------------------------------------
%%%

%% @doc Start and link a eman instance for a particular user.
%spec start_link( 'EMPUSER'() ) -> ServerSpec.
start_link( User ) ->
    process_flag(trap_exit, true),
    emplog:debug(io_lib:format("Starting eman for: ~p",[User#user.name])),
    gen_server:start_link(?REG_NAME(User), ?MODULE, [User], []).

%% @doc Stops the eman and all eprocs for a particular user.
-spec stop( 'EMPUSER'() ) -> ok.
stop( User ) ->
    emplog:debug(io_lib:format("Stopping eman for: ~p",[User#user.name])),
    gen_server:cast(?REG_NAME(User), shutdown).


%%%
%%% ---------------------------------------------------------------------------
%%% PRIVATE `gen_server` CALLBACKS
%%% ---------------------------------------------------------------------------
%%%

%% @private
%% `gen_server` callback from start_link. Initializes the gen_server and its 
%% state.
-spec init( ['EMPUSER'() ] ) -> { ok, #estate{} }.
init([User]) -> 
    pg:create(?PROJ_GROUP(User)),
    ROOT_CHECK = case empdb:get_root_user() of
                     User -> true;
                     _ -> 
                         % Requires root eman to be up.
                         eman:link_new_user( User ), 
                         false
                 end,
    {ok, #estate{user=User,is_root=ROOT_CHECK}}.

%% @private
%% `gen_server` callback. Handles synchronous calls from emp internals.
handle_call({internal, rm_eproc, Plugin}, _From, State) ->
    pg:send(?PROJ_GROUP(State#estate.user), {shutdown, Plugin}),
    {reply, ok, State};

handle_call(_Request, _From, State)-> 
    {reply, {error,"NOT IMPLEMENTED"}, State}.

%% @private
%% `gen_server` callback. Handles async calls from emp internals.
handle_cast({internal, make_seproc, User}, State) ->
    if State#estate.is_root ->
        {ok, Pid} = seproc:start_link(User, ?REG_NAME(User)),
        erlang:monitor(process, Pid),
        gen_server:cast(?REG_NAME(User), {internal, link_seproc, Pid}),
        pg:join(?SEPROJ_GROUP, Pid),
        emplog:debug("Started seproc successfully: ~p",[User#user.name]);
       true -> ok
    end,
    {noreply, State};

handle_cast({internal, link_seproc, Pid}, State) ->
    link(Pid), % The root made us a seproc, we must link!
    {noreply, State};

handle_cast({internal, make_eproc, PID, Plugin}, State) ->
    User = State#estate.user, 
    case empdb:get_subscriptions(User, Plugin) of
        {error, Res} -> 
            emplog:error("Eman:make_eproc error from empdb:",[Res]);
        Subs -> 
            Args = {User, PID, Plugin, Subs},
            {ok, Pid} = eproc:start_link(Args),
            link(Pid),
            pg:join(?PROJ_GROUP(User), Pid),
            emplog:debug("Started eproc successfully.")
    end,
    {noreply, State};

handle_cast({internal,add_sub,Module,Subscription}, State) ->
    pg:send(?PROJ_GROUP(State#estate.user), {add_sub, Module, Subscription}),
    {noreply, State};

handle_cast({sysevent, Event}, State) ->
    pg:send(?PROJ_GROUP(State#estate.user), {event, Event}),
    gman:send_event(State#estate.user, Event),
    {noreply, State};

handle_cast({event,Event}, State) ->
    pg:send(?PROJ_GROUP(State#estate.user), {event, Event}),
    if State#estate.is_root -> pg:send(?SEPROJ_GROUP, {event, Event});
       true -> ok
    end,
    gman:send_event(State#estate.user, Event),
    {noreply, State};

handle_cast( Request, State )-> 
    emplog:warn(io_lib:format("eman:cast-unknown message: ~p",[Request])),
    {noreply, State}. 

%% @private
%% `gen_server` callback. Used to capture 'EXIT' messages from seprocs and 
%% eprocs.
handle_info( {'EXIT',From,Reason}, State) -> 
    emplog:debug("eman got exit message from ~p(~p).",[From,Reason]),
    {noreply, State}; %TODO: restart *proc if it failed and wasn't killed by plugin shutdown.

handle_info(_Info, State)-> {noreply, State}.

%% @private
%% `gen_server` callback. Unused.
terminate({error, Reason}, _State) -> 
    emplog:error("eman terminated with an error:", [Reason] ),
    shutdown;
terminate(_Reason, _State)-> shutdown.

%% @private
%% `gen_server` callback. Unused.
code_change(_OldVsn, State, _Extra)-> {ok, State}.
