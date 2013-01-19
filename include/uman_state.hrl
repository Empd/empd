

%% Uman state record
-record(ustate, {user,             % emputil:#user
                 starttime=now(),  % The time the state is created (when the server starts)
                 pluginrunners=[], % PluginDef -> Pid
                 targets=[]        % Target name -> PluginDef
                }).
