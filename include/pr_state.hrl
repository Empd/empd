
-record(prstate, {
    user            :: 'EMPUSER'(),
    plugin_id       :: binary(),
    plugin_module   :: atom(),
    plugin_state    :: [ {binary(), any()} ],
    commands        :: [ 'EMPCOMMAND'() ]
}).