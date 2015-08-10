%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Dispatcher for imported anymeta content. This module can stay enabled
%%      after an Anymeta site has been imported with mod_import_anymeta.
%%		This depends on mod_import_anymeta for the creation of the the database
%%		tables.

%% Copyright 2015 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_import_anymeta_dispatch).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Import Anymeta Site - Dispatch").
-mod_description("Dispatcher for old Anymeta urls.").
-mod_prio(300).

-export([
    observe_dispatch/2
]).

-include_lib("zotonic.hrl").


%% @doc Map anyMeta URLs to Zotonic resources, uses a permanent redirect
observe_dispatch(#dispatch{path=Path}, Context) ->
    % URIs matched: 
    % index.php
    % /id/lang/slug
    % /id/123
    % (...)/id.php/(uuid|id|name)
    % (...)/(article|artefact|...)-<id>-<language>.html
    % (...)/(article|artefact|...)-<id>.html
    Parts = string:tokens(Path, "/"),
    case lists:reverse(Parts) of
        ["index.php"] ->
            ContextQs = z_context:ensure_qs(Context),
            redirect_rsc(m_rsc:rid(page_home, ContextQs), z_context:get_q("lang", ContextQs), ContextQs);
        [AnyId,"id.php"|_] ->
            redirect(AnyId, undefined, Context);
        [AnyId,"id"] ->
            case z_utils:only_digits(AnyId) of
                true -> redirect(AnyId, undefined, Context);
                false -> undefined
            end;
        [Slug, [_,_] = Lang, [C|_] = AnyId] when C >= $0; C =< $9 ->
            case z_utils:only_digits(AnyId) of
                true -> redirect(AnyId, Lang, Context);
                false -> old_anymeta_url(Slug, Context)
            end;
        [Rsc|_] ->
            old_anymeta_url(Rsc, Context);
        [] ->
            undefined
    end.

old_anymeta_url(Rsc, Context) ->
    case filename:extension(Rsc) of
        ".html" ->
            case string:tokens(filename:rootname(Rsc), "-") of
                [_Kind,AnyId,[_,_] = Lang] ->
                    redirect(AnyId, Lang, Context);
                [_Kind,AnyId] ->
                    redirect(AnyId, undefined, Context);
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

redirect(AnyId, Lang, Context) ->
    case z_db:q1("select rsc_id from import_anymeta where anymeta_id = $1", 
                 [z_convert:to_integer(AnyId)], 
                 Context)
    of
        undefined -> 
            undefined;
        RscId when is_integer(RscId) ->
            redirect_rsc(RscId, Lang, Context)
    end.

redirect_rsc(undefined, _Lang, _Context) ->
    undefined;
redirect_rsc(RscId, Lang, Context) ->
    Lang1 = map_language(Lang),
    Context1 = case z_trans:is_language(Lang1) of
                 true ->
                     % Add language
                     z_context:set_language(list_to_atom(Lang1), Context);
                 false ->
                     % Ignore language
                     Context
               end,
    case m_rsc:p(RscId, page_url, Context1) of
        undefined ->
            undefined;
        URL ->
            {ok, #dispatch_match{
                mod=controller_redirect,
                mod_opts=[{url, URL}, {is_permanent, true}],
                bindings=[]
            }}
    end.

map_language("jp") -> "ja";
map_language(<<"jp">>) -> <<"ja">>;
map_language(Code) -> Code.
