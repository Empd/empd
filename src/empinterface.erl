%% EmpInterface.erl 
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
%%
%% @doc 
%%  Library to generalize the EMP Interface API so that Interfaces can focus
%%  on the protocol they are supporting rather than the API issues. It also 
%%  wraps the ejson module so the protocol is abstracted away from interface
%%  code. 
%% @end
%% ----------------------------------------------------------------------------
-module(empinterface).

% Encoding messages for sending back to users.
-export([encode_event/1,encode_error/3,
         encode_reply/3,encode_cast/3,encode_update/3]).

% Handle functions that send a message into emp for handling.
-export([handle_async/3, handle_sync/2]).

% Event Packet type, used to reduce the amount of pattern matching until the 
% last second.
-export_type([event_packet/0, utilret/0, urgency/0, json_valid_value/0]).

-type event_packet() :: { binary(), binary(), binary() }.
-type urgency() :: [0..9].
-type json_valid_value() :: null 
                          | boolean() 
                          | integer() 
                          | float() 
                          | {[ {binary(), json_valid_value()} ]} 
                          | [ json_valid_value() ].
-type msgtarget() :: null | binary().
-type value() :: json_valid_value().
-type utilret() :: shutdown 
                 | {util, auth_success, 
                    UserName :: list(), SessionId :: binary()} 
                 | {error, invalid_session} 
                 | {error, Reason :: binary()} 
                 | {ok, Return :: json_valid_value()}.

%% @doc 
%%  Encodes an event packet, that you recieved from gman, for sending to the 
%%  client over the wire.
%% @end 
-spec encode_event( event_packet() ) -> binary().
encode_event( _Event = {Target, EventName, Value} ) ->
    ejson:encode( 
      {[
        {<<"type">>,   <<"event">>},
        {<<"target">>, Target},
        {<<"event">>,  EventName},
        {<<"value">>,  Value} 
       ]}).

%% @doc Encode an error message for sending to the client.
-spec encode_error( urgency(), msgtarget(), value() ) -> binary().
encode_error( Urgency, Target, Value ) ->
    encode_util(Urgency, <<"error">>, Target, Value).

%% @doc Encode a command or query reply for sending to the client.
-spec encode_reply( urgency(), msgtarget(), value() ) -> binary().
encode_reply( Urgency, Target, Value ) ->
    encode_util(Urgency, <<"reply">>, Target, Value).

%% @doc Encode a system cast for sending to the client.
-spec encode_cast( urgency(), msgtarget(), value() ) -> binary().
encode_cast( Urgency, Target, Value ) ->
    encode_util(Urgency, <<"cast">>, Target, Value).

%% @doc Encode a update message for sending to the client.
-spec encode_update( urgency(), msgtarget(), value() ) -> binary().
encode_update( Urgency, Target, Value ) ->
    encode_util(Urgency, <<"update">>, Target, Value).

%% @hidden
%% Wrapper method, as all above encoding methods have the same structure.
-spec encode_util( urgency(), binary(), msgtarget(), value() ) -> binary().
encode_util( Urgency, Type, Target, Value ) ->
    ejson:encode( 
      {[
        {<<"urgency">>, Urgency},
        {<<"type">>,    Type},
        {<<"target">>,  Target},
        {<<"value">>,   Value} 
       ]}).

%% @doc 
%%  Decodes and then handles the message asyncronously given the session 
%%  id and which interface to send the reply back to. If it errors back
%%  this means the message you tried to handle was a login message (which
%%  isn't allowed to be run in async mode).
%% @end
-spec handle_async( binary(), pid(), binary() ) -> 
          ok | {error, no_async_logins}.
handle_async( Data, InterfacePid, Session ) ->
    Message = ejson:decode( Data ),
    handle_msg_async(Message, InterfacePid, Session).
			
%% @doc
%%  Decodes and then handles the message syncronously given the session id.
%%  Any errors you recieve back are safe to encode and send straight back 
%%  to the client.
%% @end
-spec handle_sync( binary(), binary() ) -> utilret().
handle_sync( Data, Session ) ->
    Message = ejson:decode( Data ),
    handle_msg( Message, Session ).


%% ========================================================================
%% Private Functions
%% ========================================================================

%% @private
%% @doc Send the results to a PID using the messaging system.
-spec handle_msg_async( json_valid_value(), pid(), binary() ) -> 
          ok | {error, no_async_logins}.
handle_msg_async({[ {<<"login">>, _L} ]},A, _S) ->
    A ! {error, no_async_logins};
handle_msg_async( Message, AsyncPid, Session ) ->
    spawn( fun() -> AsyncPid ! handle_msg( Message, Session ) end ),
    ok.


%% @private
%% @doc 
%%  Handles the decoded json message from handle_sync. See IAPI for
%%  details regarding what types of JSON messages are accepted. 
%% @end
-spec handle_msg( json_valid_value(), SessionId :: binary() ) -> utilret().
handle_msg( {[ {<<"login">>, LoginData} ]}, _NullSession ) ->%%
    % Session variable must be a UUID. LoginData can be either a tuple of 
    % {username, password}, or some RSA public key pair, etc.
    case LoginData of
        {[ 
          {<<"username">>, UserName},
          {<<"passhash">>, PassHash}
         ]} -> 
            UName = makebinary(UserName),
            PHash = makebinary(PassHash),
            case gman:login(UName, PHash) of
                {true, RName, SessionId} -> {util, auth_success,
                                              makestring(RName),
                                              SessionId};
                {error, Reason} -> {error, Reason}
            end;
        
        {[
          {<<"pubkey">>, PubKey}
         ]} -> 
            PK = makestring(PubKey),
            case gman:login({rsa,PK}) of
                {true, UName, SessionId} -> {util, auth_success,
                                              makestring(UName), 
                                              SessionId};
                {error, Reason} -> {error, Reason}
            end
    end;
handle_msg( {[ {<<"logout">>, _SidTag} ]}, Session ) -> 
    gman:logout( makebinary(Session) ), 
    shutdown;

% Pick off the sid pair and reorder it so that the following work.
handle_msg( {[ {<<"session">>, SidPair}| Rest ]}, Session) ->
    handle_msg( {Rest++[{<<"session">>,SidPair}]}, Session);

% Running Command.
handle_msg( {[ {<<"command">>, CMD}| _SidTag]}, Session ) ->
    case CMD of
        {OptionList} -> runCmd( Session, OptionList );
        CMDS -> runCmds(Session, CMDS )
    end;
  
% Running Queries.
handle_msg( {[ {<<"query">>, {[ QUERY ]}}| _SidTag ]}, Session ) -> 
    gman:run_query( makebinary(Session), QUERY);

handle_msg( Msg, Session ) ->
    emplog:warn( io_lib:format(
       "interface:handle_msg(session=~p) doesnt know this type of message: ~p",
                   [ makestring(Session), 
                     makestring(Msg)
                   ])),
    {error, <<"unknown packet">>}.


%% @private
%% @doc 
%%  Private functionality for running commands. Can take a list of commands, 
%%  but will only return the result of the last command in the list.
%% @end
runCmds(_Session, []) -> ok;
runCmds(Session, [ {OptionList} | Rest ]) ->
    spawn( fun() -> runCmd(Session, OptionList) end ),
    runCmds(Session, Rest).
runCmd(Session, OptionList ) ->
    try
        {<<"target">>, Target} = lists:keyfind(<<"target">>, 1, OptionList),
        {<<"cmd">>, Command} = lists:keyfind(<<"cmd">>, 1, OptionList),
        {<<"params">>, ParamList} = lists:keyfind(<<"params">>, 1, OptionList),
        gman:run_cmd(makebinary(Session), 
                     cleantarget(Target),
                     Command, 
                     ParamList)
    
    %Fails if keyfinds return false.
    catch _:_ -> {error, <<"Invalid match">>} end.


%% ========================================================================
%% Internal Functions
%% ========================================================================

%% @hidden clean the incoming target and make it binary or `null`.
cleantarget( null ) -> null;
cleantarget(<<"null">>) -> null;
cleantarget(T) -> makebinary(T).

%% @hidden Module's type conversions for various input.
makestring( Val ) when is_binary(Val) -> binary_to_list(Val);
makestring( Val ) when is_atom(Val)   -> atom_to_list( Val );
makestring( Val ) -> Val.
makebinary( Val ) when is_list(Val) -> list_to_binary(Val);
makebinary( Val ) when is_binary(Val) -> Val.
