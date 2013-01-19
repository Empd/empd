%% EMPInternal.hrl
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
%%  Internal definitions and records that arn't used anywhere else.
%% @end
%% ----------------------------------------------------------------------------

-include("emptypes.hrl").

-define(PROCESS_NAME(A,B),
        erlang:binary_to_atom(<<(A)/binary,(B)>>,latin1)).

-type encodable_return() :: {ok, Result :: json_valid_value()} 
                          | {error, Reason :: binary()}.

% A mapping from event to command.
-record(subscription, 
        {
          event :: 'EMPEVENT'(),
          command :: 'EMPCOMMAND'(),
          temp=false :: boolean(),
          mapping=[] :: [tuple()]
        }).
-type 'EMPSUBSCRIPTION'() :: #subscription{}.
-define(EMPSUBSCRIPTION(EVENT,COMMAND,MAPPING),
       #subscription{
          event=EVENT,
          command=COMMAND,
          mapping=MAPPING           
       }).

% User data to pass around, its crucial to map a uid to a
% string of the user's name. This name is used for  
% registration of user nodes, the uid is for keying searches
% for the user processes.
-record(user, 
        {
          uid         :: 'UUID'(),
          name=(<<>>) :: 'BINARYSTRING'() %Note: default required.
        }).
-type 'EMPUSER'() :: #user{}.
-define(EMPUSER( UID, NAME ),
        #user{
            uid=UID,
            name=NAME
        }).

%%% Plugin Definition:
% Only used in two places: pluginrunner/pman and empdb.
% On start up, pman grabs from empdb the old plugindefs and
% launches pluginrunners. Then on shutdown it gathers them
% back up and saves them to empdb.
-record(plugindef,
        {
         id        :: 'UUID'(),
         module    :: atom(),
         state     :: [ { 'BINARYSTRING'(), any() } ],
         commands  :: 'EMPCOMMAND'()
        }).
-type 'EMPPLUGINDEF'() :: #plugindef{}.
-define(EMPPLUGINDEF(ID, MODULE, STATE, COMMANDS ),
        #plugindef{
            id = ID,
            module = MODULE,
            state = STATE,
            commands = COMMANDS
        }).
