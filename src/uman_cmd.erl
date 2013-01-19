%% UMan_Cmd.erl
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
%%  This Module contains the functions responsible for handling UMan's 
%%  System command handlers. Any commands that EMP displays when you
%%  type 'help', even the help command itself, is parsed and handled
%%  here.
%% @end
%% ----------------------------------------------------------------------------
-module(uman_cmd).
-include("uman_state.hrl").

% The types of commands and queries avaliable to UMan.
-export([handle_plugin_cmd/4, handle_cmd/3, handle_cmd/4, handle_query/3]).

-type sysquery() :: tuple().
-type json_valid_value() :: null | boolean() | integer() | float() 
                          | {[ {binary(), json_valid_value()} ]} 
                          | [ json_valid_value() ].
-type encodable_return() :: {ok, Result :: json_valid_value()} 
                          | {error, Reason :: binary()}.

-define(COMMAND_LIST, [ <<"help">>,
                        <<"list">>,
                        <<"register">>,
                        <<"unregister">>,
                        <<"subscribe">>,
                        <<"unsubscribe">>,
                        <<"plug">>,
                        <<"trigger">>
                      ]).

-define(QUERY_LIST, [ <<"status">>,
                      <<"varstate">>
                     ]).

%%
%% ============================================================================
%% Public Message Handlers
%% ============================================================================
%%


%% @doc 
%%  Handle a System Query. These are sent in from interfaces to ask about
%%  the state of EMP and/or plugins.
%% @end
-spec handle_query( sysquery(), pid(), #ustate{} ) -> ok.
handle_query( {<<"status">>,Target}, From, State ) ->
    case Target of
        <<"emp">> -> gen_server:reply(From, {ok, genstatus(State)});
        _Plugin   -> gen_server:reply(From, {error, <<"NotImplemented">>})
    end, ok;
handle_query( {<<"varstate">>,Var}, From, _State ) ->
    try
        Key = binary_to_existing_atom(Var, unicode),
        case application:get_key(emp,Key) of
            undefined -> gen_server:reply(From,  {error, <<"Unknown key">>});
            {ok, Val} -> gen_server:reply(From, {ok, Val})
        end
    catch _:_ -> gen_server:reply(From,  {error, <<"Unknown key">>}) end,
    ok;
handle_query( _Msg, From, _State ) ->
    gen_server:reply(From, {error,<<"Unknown query.">>}), ok.


%% @doc Handles running a command on a plugin module.
-spec handle_plugin_cmd( pid(), binary(), [ json_valid_value() ], pid() ) -> ok.
handle_plugin_cmd(Plugin, Command, Params, From) -> 
    gen_server:reply(From, handle_plugin_cmd(Plugin, Command, Params)),
    ok.


%% @hidden The private function for handling the plugin message.
handle_plugin_cmd(PluginId, Command, Params) ->
    PluginId ! {external, run_cmd, self(), Command, Params},
    receive Response -> Response end.


%% @doc Handle a System command from an interface and send the result back to a given PID.
-spec handle_cmd( binary(), [ json_valid_value() ], pid(), #ustate{} ) -> #ustate{}.
handle_cmd( Command, Params, From, State ) ->
    {Ret, NewState} = handle_cmd(Command, Params, State),
    gen_server:reply(From, Ret), 
    NewState.


%% @doc Handle a System command and return a tuple of its return value and the new state.
-spec handle_cmd( binary(), [ json_valid_value() ], #ustate{} ) -> { encodable_return(), #ustate{} }.
handle_cmd( <<"help">>, _Params, State ) -> %getting help on commands 
    {{ok, <<"Help Command isn't implemented at the moment.">>}, State};
handle_cmd( <<"list">>, [ListType|_RestParams], State ) -> %for listing plugins, subscriptions, or commands.
    case ListType of
        <<"cmds">>     -> {{ok,?COMMAND_LIST}, State};
        <<"commands">> -> {{ok,?COMMAND_LIST}, State};
        <<"queries">>  -> {{ok,?QUERY_LIST}, State};
        <<"plugins">> ->
            {Res,_} = lists:unzip( State#ustate.targets ),
            {{ok,Res}, State};
        _Else_ -> {{ok, <<"Only able to list 'commands', 'plugins' or 'queries'.">>}, State}
    end;
handle_cmd( <<"register">>, Params, State ) -> % Register a plugin with EMP
    try
        [PluginModule,Path|_] = Params,
        case code:add_pathz(binary_to_list(Path)) of
            {error, bad_directory} -> {{error,<<"Bad Directory.">>}, State};
            true -> load_register( State, PluginModule );
            M -> emplog:debug("Bad return from add path: ~p",[M])
        end
    catch 
        _:R ->
            emplog:debug("Error loading module: ~p",[R]),
            {{error,<<"Error loading module, see 'help' command.">>}, State}
    end;

handle_cmd( <<"unregister">>, _Params, State ) -> %Unregister a plugin with EMP
    {{ok, <<"Not Implemented">>}, State};
handle_cmd( <<"subscribe">>, _Params, State ) -> %command to subscribe.
    {{ok, <<"Not Implemented">>}, State};
handle_cmd( <<"unsubscribe">>, _Params, State ) -> %command to unsubscribe.
    {{ok, <<"Not Implemented">>}, State};
handle_cmd( <<"plug">>, _Params, State ) -> %get plugin info: cmds,events,issue
    {{ok, <<"Not Implemented">>}, State};
handle_cmd( <<"trigger">>, _Params, State ) -> %manual triggering of events
    {{ok, <<"Not Implemented">>}, State};

handle_cmd( _Command, _Params, State ) -> 
    {{error, <<"Unknown command!">>}, State}.


%%
%% ============================================================================
%% Private Command functionality.
%% ============================================================================
%%

%% @hidden Utility function for 'status' command for generating the status 
%%          message.
genstatus( State ) ->
    {{Y,M,D},{H,MM,S}} = 
        calendar:now_to_universal_time(State#ustate.starttime),
    NumPlugs = length(State#ustate.pluginrunners),
      list_to_binary(
        io_lib:format(
          "Emp is running!~n"++
          "Up since: ~p-~p-~pT~p:~p:~pZ~n"++
          "Number of Plugins: ~p~n",
          [
           Y,M,D,H,MM,S, 
           NumPlugs
           ])).

%% @hidden WARNING: THIS WILL CREATE ATOMS! It calls load_into_db, only if the
%%           module can be loaded into the vm.
load_register( State, PluginModule ) ->
    Module = erlang:binary_to_atom(PluginModule, latin1),
    emplog:debug("NOTE! Created atom(~p).",[Module]),
    case catch code:load_file( Module ) of
        {module, Module} -> load_into_db( State, Module );
        
         % uh oh... we have just created an atom for nothing.
        {error, undef} ->
            {{error, <<"That module does not exist.">>}, State};
        {error, Reason} ->
            emplog:debug("LoadRegister failed: ~p",[Reason]),
            {{error, <<"Could not parse plugin name.">>}, State};
        {'EXIT',{badarg,_}} -> 
            {{error, <<"Could not find the moudle in the path.">>}, State}
    end.

%% @hidden Runs the registration functions on the new module, and registers 
%%          the plugin in the db.
%% load_into_db( #ustate{}, atom() ) -> { Ret :: tuple(), #ustate{} }
load_into_db( State, Module ) ->
    {Module,Info} = apply( Module, registration_info, [] ),
    Cmds   = apply( Module, register_cmds, [] ),
    Events = apply( Module, register_events, [] ),
    empdb:register_plugin(Module, Info, Cmds, Events),
    {{ok, <<"registered, to start plugin: restart or use 'start' cmd.">>}, 
     State
    }.
    