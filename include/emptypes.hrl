%% EMPTypes.hrl
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
%%  Type system for EMP. These are all of the generic types that are both used
%%  internally and in plugins.
%% @end
%% ----------------------------------------------------------------------------

%%
%% ==================================================================
%% Emp Standard Objects
%% ==================================================================
%%

-type 'STATEID'() :: pid().

-type 'UUID'()    :: binary().
-define(UUID(), empdb:gen_uuid()).

-type 'BINARYSTRING'() :: binary().

-type 'EMPTYPEDEF'() :: [0..3] 
                      | [4..5 | 'EMPTYPEDEF'()] 
                      | [6 | atom()] 
                      | [7 |binary()] 
                      | [8] 
                      | [9 | atom() | 'EMPTYPEDEF'()].
%%
%% EMP Type definitions for functions, commands and event variables/parameters.
%%  Use according to recommendations and APIs. These are each a separate type,
%% and should not be conncatinated, instead just add them all to a list. (ie
%% [?EMP_INT,?EMP_STRING], etc.)
%%

%% BLANK, IGNORE should be used sparingly and primarily for testing.
-define(EMP_IGNORE, [0]).
-define(EMP_BLANK,  [0]).

%% STRINGs, NUMBERs, and BOOLEANs are the only base types as they are the only
%%  base types in JSON.
-define(EMP_STRING, [1]).
-define(EMP_NUMBER, [2]).
-define(EMP_BOOLEAN,[3]).

%% LISTs can be used like this: ?EMP_LIST(?EMP_STRING) which is a list of 
%%  strings.
-define(EMP_LIST(TYPE), [4 | TYPE]).

%% DICTs are dictionaries of type String->TYPE, where TYPE is what you specify.
%%  They are constructors and can be treated like EMP_LIST.
-define(EMP_DICT(TYPE), [5 | TYPE]).

%% URLs are a special type of string that can be interpreted like a URL by an
%% interface. It is a constructor type and must take a UTYPE from below. URLs
%% should only be used if the URL type is constant! Otherwise, use a 
%% EMP_STRING.
-define(EMP_URL(UTYPE), [6,UTYPE]).

%% FILEs are large blobs of "text" that can be binary data. It doesn't matter 
%% what type of file is being passed as long as it is given a proper extension.
-define(EMP_FILE(EXTENSION), [7,EXTENSION]).

%% TYPESIGs are the type signature of a type signature and can be used for 
%%  defining your own types.
-define(EMP_TYPESIG, [8]).

%% UNIQUEs are macro types for plugins and cannot be compared to other generic
%%  type signatures. This allows plugins to make interface specific return 
%%  types that only certain interfaces can interpret. Warning, generic 
%%  interfaces will treat UNIQUEs as EMP_STRINGs.
-define(EMP_UNIQUE(MODULENAME,TYPESIG),  [9,MODULENAME,TYPESIG]).


%% ================
%% EMP_UTYPES:
%%   These are URL types that all interfaces are aware of. That way they can 
%%   possibly handle the URL in a special way.
-define(UTYPE_PIC, pic).
-define(UTYPE_LINK, link).
-define(UTYPE_AUDIO, audio).
-define(UTYPE_VIDEO, video).



-type json_valid_value() :: null | boolean() | integer() | float() 
                          | {[ {binary(), json_valid_value()} ]} 
                          | [ json_valid_value() ].

% Parameters for checking type signatures, merging default 
% values and inputs. 
-record(param, 
        { 
          name    :: 'BINARYSTRING'(),
          default :: json_valid_value(), 
          type    :: 'EMPTYPEDEF'()
        }).
-type 'EMPPARAM'() :: #param{}.
-define(EMPPARAM(Name, TypeDef, Default), 
            #param{name=Name,
                   type=TypeDef, 
                   default=Default}).

% A Command object that can be passed around and received 
% from the database. Used for mapping event values to command
% parameters in subscriptions.
-record(command,
        {
          pid  :: 'UUID'(),
          cid  :: 'UUID'(),
          name :: 'BINARYSTRING'(),
          func :: fun(),
          dparams=[] :: ['EMPPARAM'()],
          rettype :: 'EMPTYPEDEF'()
        }).
-type 'EMPCOMMAND'() :: #command{}.
-define(EMPCOMMAND(Name, Function, ParamList, RetList), 
            #command{name=Name, 
                     func=Function, 
                     dparams=ParamList, 
                     rettype=RetList}).

% Event objects that can be passed around and received from
% the database. Used for mapping event values to commands 
% in subscriptions.
-record(event, 
        {
          pid :: 'UUID'(),
          eid :: 'UUID'(),
          name :: 'BINARYSTRING'(),
          dvalues=[] :: ['EMPPARAM'()] 
        }).
-type 'EMPEVENT'() :: #command{}.
-define(EMPEVENT(Name, ValueList),
            #event{name=Name, 
                   dvalues=ValueList}).
