%% EMPD_App.erl
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
%% @see empd. <em>EMP Daemon</em>
%% @doc 
%%  The entry point for the EMP core server. This is an application behaviour
%%  and starts the emp daemon which is the core's primary supervisor. <br/>
%%  This application requires both the emplog and empdb applications to first
%%  be running.
%% @end
%% ----------------------------------------------------------------------------
-module(empd_app).
-behaviour(application).

%% Application callbacks
-export([start_link/2, start/2, prep_stop/1, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start( StartType, StartArgs ) ->
    start_link( StartType, StartArgs).

start_link(_StartType, StartArgs) ->
    update_env( empdb:emp_get_vars() ), % load old environment
    empd_sup:start_link(StartArgs). % start daemon

% called before killing empd
prep_stop( _State ) ->
    emplog:debug("empd stopping...").

% called after killing empd, using for clean up.
stop(_State) ->
    push_env(). % save current environment


%%
%% ===================================================================
%% Environment Perseverance
%% ===================================================================
%%

% Updates the current env with a list.
update_env( {ok, List} )-> 
    update_env(List);
update_env( {error, Reason} ) ->
    emplog:error("EmpD Unable load previous environment, using predefined.",
                  [Reason]),
    ok;

update_env( [] ) -> 
    ok;
update_env( [{Key, Val}|Rest] ) ->
    application:set_env(emp, Key, Val),
    update_env(Rest).


%Saves the current environment in the empdb.
push_env() ->
    Env = application:get_all_env(emp),
    push_env(Env).

push_env( [] ) -> ok;
push_env( [{Key,Val}|Rest] ) ->
    empdb:emp_set_var(Key, Val),
    push_env(Rest).
                    
