%% GMan.erl 
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
%% @see uman_serve. <em>User Manager</em>
%% @see empinterface. <em>Interface API Module</em>
%%
%% @doc 
%%  The GMan is the govener between the user configurations and plugins, and 
%%  the outside world. It handles all authentication as well as securely 
%%  passing messages to and from a user's uman.
%% @end
%% ----------------------------------------------------------------------------
-module(gman).
-include("empinternal.hrl").

-record(gstate,{
            sessions=dict:new(), % SessionId -> {Username, Pid}
            uservers=dict:new()  % Username -> Pid
           }).

-define(REG_NAME, {global, empgman}).

% Interface exports
-export([login/1,login/2, logout/1, run_cmd/4, run_query/2]).
% GMan Server exports
-export([start_link/0]).
% Uman accessors
-export([add_uman/2]).
% Eman accessors
-export([send_event/2]).

% Private exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%TODO: some of these types are in empinterface.erl too, perhaps put inside a header?
-type msgtarget() :: null | binary().
-type loginresult() :: {true, UserName :: binary(), SessionId :: binary()} | {error, Reason :: binary()}.
-type utilret() :: shutdown | {util, auth_success, UserName :: list(), SessionId :: binary()} |
                    {error, invalid_session} | {error, Reason :: binary()} | {ok, Return :: json_valid_value()}.


%%%
%%% ============================================================================
%%% Server Exports
%%% ============================================================================
%%%

%% @doc Start the Gman server.
start_link() -> 
    gen_server:start_link(?REG_NAME, ?MODULE, [], []).


%% @private
%% @doc `gen_server' callback for start_link.
init( [] ) -> %There is no saved state to reload, its all runtime based.
    {ok, #gstate{}}.
	
%%%
%%% ============================================================================
%%% Functions for Internal UMAN Communication
%%% ============================================================================
%%%

%% @doc Register the user with the GMan so that it knows who can log in.
-spec add_uman( 'EMPUSER'(), pid() ) -> ok.
add_uman( User, Pid ) ->
    gen_server:cast(?REG_NAME , {internal, add_uman, {User, Pid}}).

%%%
%%% ============================================================================
%%% Functions for Internal EMAN EVENT Communication
%%% ============================================================================
%%%

%% @doc Broadcasts the event to all Eprocs for a given User.
-spec send_event( 'EMPUSER'(), 'EMPEVENT'() ) -> ok.
send_event( User, Event ) ->
    gen_server:cast(?REG_NAME , {external, event, User, Event}).


%%
%% ============================================================================
%% Functions for Interface Clients
%% ============================================================================
%%

%% @doc Register a client using a rsa public key.
-spec login( {rsa, PUBKEY :: binary()} ) -> loginresult(). 
login( {rsa, PUBKEY} ) ->
    emplog:debug("User attempted login with public key(~p)", [PUBKEY]),
    case empdb:user_login( PUBKEY ) of
        {ok, UserName, SessionId} -> 
            add_sessionId(UserName, SessionId, self()), 
            {true, UserName, SessionId};
        {error, Reason} -> {error, Reason};
        %false -> {error, "Invalid Username or password"};
        Msg ->
            emplog:warn("gman:login/1-Unknown return from user_login: ~p",[Msg]),
            {error, "Unknown error"}
    end.

%% @doc Register a client using a Username and password hash.
-spec login( UserName :: binary(), PassHash :: binary() ) -> loginresult().
login( UserName, PassHash ) ->
    emplog:debug("User attempted login with username(~p) and password(~p)",
                               [UserName, PassHash]),
    case empdb:user_login(UserName, PassHash) of
        {ok, Uname, SessionId} -> 
            add_sessionId(Uname, SessionId, self()),
            {true, UserName, SessionId};
        {error, Reason} -> {error, Reason};
        %false -> {error, "Invalid Username or password"};
        Msg -> 
            emplog:warn("gman:login/2-Unknown return from user_login: ~p",[Msg]),
            {error, "Unknown error"}
    end.

%% @doc Unregister a client and remove the sessionid from the session list.
-spec logout( UUID :: binary() ) -> ok | {error, Reason :: binary()}.
logout( SessionId ) ->
    case rm_sessionId( SessionId ) of
        {ok, _UserName} -> empdb:user_logout(SessionId);
        {error, Reason} -> {error, Reason};
        Msg ->
            emplog:warn("gman:logout/1-Unknown return from rm_sessionId: ~p",[Msg]),
            {error, "Unknown error"}
    end.

%% @doc 
%%  Ask to run a command on the user's uman. If the target is not null, 
%%  it will be run as a plugin command.
%% @end
-spec run_cmd( UUID :: binary(), msgtarget(), binary(), [binary()] ) -> utilret().
run_cmd( SessionId, Target, Command, ParamList ) ->
    gen_server:call(?REG_NAME , {external, system_command,
                            {SessionId, Target, Command, ParamList}}).

%% @doc
%%  Similar to run_cmd, it will ask to run a command on the user's uman.
%%  All queries are system queries, so theres no possibility of the target
%%  not being there. However, queries can require permissions.
%% @end
-spec run_query( UUID :: binary(), binary() ) -> utilret().
run_query( SessionId, Query ) ->
    gen_server:call(?REG_NAME , {external, system_query, {SessionId, Query}}).	


%%	
%% ============================================================================
%% Internal Server Functionality
%% ============================================================================
%%	
	
%% @private
%% @doc Handles most internal communication that talks to gman.
handle_cast( {internal, add_session, {Name,Session,Pid}}, State )->
    ActiveSessions = dict:store(Session, {Name,Pid}, State#gstate.sessions),
    {noreply, State#gstate{sessions=ActiveSessions}};

handle_cast({internal, add_uman, {User, Registration}}, State ) ->
    UmanServers = dict:store(User,Registration,State#gstate.uservers),
    {noreply, State#gstate{uservers=UmanServers}};

handle_cast({internal, shutdown}, State)->
    {stop, shutdown, State};

handle_cast({external, event, User, Packet}, State ) ->
    spawn( fun() ->
            {_, Interfaces} = lists:unzip(dict:to_list(State#gstate.sessions)),
            UName = User#user.name,
            lists:foreach(fun ({U,P})->
                            case U of 
                                UName -> try P!{event,Packet} catch _:_ ->ok end;
                                _ -> emplog:debug("Tried to send user an event, but user has no interface: ~p",[Interfaces])
                            end
                          end, Interfaces)
           end),
    {noreply, State};

handle_cast( Msg, State ) -> 
    emplog:warn(io_lib:format("gman:cast-Unknown message: ~p",[Msg])),
    {noreply, State}.

%% @private
%% @doc A `gen_server' callback for removing sessions and handle external messages.
handle_call({internal, rm_session, [Session]}, _From, State ) ->
    ActiveSessions = State#gstate.sessions,
    case dict:find(Session, ActiveSessions) of
        {ok, {Name,_}} ->
            NewState = State#gstate{sessions=dict:erase(Session, ActiveSessions)},
            {reply, {ok, Name}, NewState};
         _ -> 
            {reply, {error, "Session Invalid"}, State}
    end;

handle_call(Msg, From, State)->
    spawn( fun()-> handle_external_msg(State, From, Msg) end ),
    {noreply, State}. 

%% @private
%% @doc Unused `gen_server' callback.
handle_info(_Info, State)-> {noreply, State}.

%% @private
%% @doc Unused `gen_server' callback.
terminate({error, Reason}, _State) -> 
    emplog:error("gman:termination with error", [Reason]),
    shutdown;

terminate(_Reason, _State)-> 
    emplog:debug("gman:terminating normal..."),
    shutdown.

%% @private
%% @doc Unused `gen_server' callback.
code_change(_OldVsn, State, _Extra)-> {ok, State}.


%%%
%%% ---------------------------------------------------------------------------
%%% Internal server communication.
%%% ---------------------------------------------------------------------------
%%%

%% @hidden Adds a given session id paired to a User and Client PID.
add_sessionId( UserName, SessionId, InterfacePid ) ->
    gen_server:cast(?REG_NAME , {internal, add_session, {UserName, SessionId, InterfacePid}}).

%% @hidden Removes the session id from the running gman server.
rm_sessionId( SessionId ) ->
    gen_server:call(?REG_NAME , {internal, rm_session, [SessionId]}).

%% @hidden Handles a command from an external source.
handle_external_msg(State, From, Msg) ->
    case Msg of
        {external, system_command, {Session, Target, Command, ParamList}} ->
            case dict:find(Session, State#gstate.sessions) of
                {ok, {User,_}} -> % TODO: Validate user permissions for running command?
                    gen_server:reply(From, uman_serve:run_command(User, Target, Command, ParamList));
                _ -> 
                    gen_server:reply(From, {error, invalid_session})
            end;
        
        {external, system_query, {Session,Query}} ->
            case dict:find(Session, State#gstate.sessions) of
                {ok, {User,_}} -> % TODO: Validate user permissions for running query.
                    gen_server:reply(From, uman_serve:run_query(User, Query));
                _ -> 
                    gen_server:reply(From, {error, invalid_session})
            end;
        _ -> 
            emplog:error("gman:call-Unknown message received: ~p",[Msg])
    end.