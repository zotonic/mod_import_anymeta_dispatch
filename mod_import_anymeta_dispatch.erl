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
-mod_prio(290).

-export([
    observe_dispatch/2,
    observe_dispatch_rewrite/3,
	observe_dispatch_host/2
]).

-include_lib("zotonic.hrl").

%% @doc Anymeta has external uris "id/123" coming in from external sites
%%      This clashes with the 'id' dispatch rule, to be able to override the
%%      dispatch rule we have to rewrite "id/123" requests to something we will catch
%%      
observe_dispatch_rewrite(#dispatch_rewrite{is_dir=_IsDir}, {Parts, Args} = Dispatch, _Context) ->
    case Parts of
        ["id", [C|_] = AnyId] when C >= $0, C =< $9 ->
            {["anymetaid",AnyId], Args};
        _ ->
            Dispatch
    end.

%% @doc Map anyMeta URLs to Zotonic resources, uses a permanent redirect
observe_dispatch(#dispatch{host=Host, path=Path}, Context) ->
    % URIs matched: 
    % index.php
    % /id/lang/slug
    % /id/lang/
    % /id/123
    % /search/123
    % /search/123/nl
	% /person/123
	% /person/123/nl
    % (...)/id.php/(uuid|id|name)
    % (...)/(article|artefact|...)-<id>-<language>.html
    % (...)/(article|artefact|...)-<id>.html
    Parts = string:tokens(Path, "/"),
    case lists:reverse(Parts) of
        ["index.php"] ->
            ContextQs = z_context:ensure_qs(Context),
            redirect_rsc(m_rsc:rid(page_home, ContextQs), z_context:get_q("lang", ContextQs), ContextQs);
        [AnyId,"id.php"|_] ->
            redirect(Host, AnyId, undefined, Context);
        [AnyId,"anymetaid"] ->
            redirect(Host, AnyId, undefined, Context);
        % "id/123" will never arrive here due to the 'id' dispatch rule in mod_base
        % [AnyId,"id"] ->
        %     case z_utils:only_digits(AnyId) of
        %         true -> redirect(AnyId, undefined, Context);
        %         false -> undefined
        %     end;
        [[_,_] = Lang, [C|_] = AnyId, "search"] when C >= $0, C =< $9 ->
            case z_utils:only_digits(AnyId) of
                true -> redirect(Host, AnyId, Lang, Context);
                false -> undefined
            end;
        [[C|_] = AnyId, "search"] when C >= $0, C =< $9 ->
            case z_utils:only_digits(AnyId) of
                true -> redirect(Host, AnyId, atom_to_list(z_context:language(Context)), Context);
                false -> undefined
            end;
        [[_,_] = Lang, [C|_] = AnyId, "person"] when C >= $0, C =< $9 ->
            case z_utils:only_digits(AnyId) of
                true -> redirect(Host, AnyId, Lang, Context);
                false -> undefined
            end;
        [[C|_] = AnyId, "person"] when C >= $0, C =< $9 ->
            case z_utils:only_digits(AnyId) of
                true -> redirect(Host, AnyId, atom_to_list(z_context:language(Context)), Context);
                false -> undefined
            end;	
        [Slug, [_,_] = Lang, [C|_] = AnyId] when C >= $0, C =< $9 ->
            case z_utils:only_digits(AnyId) of
                true -> redirect(Host, AnyId, Lang, Context);
                false -> old_anymeta_url(Host, Slug, Context)
            end;
        [[_,_] = Lang, [C|_] = AnyId] when C >= $0, C =< $9 ->
            case z_utils:only_digits(AnyId) of
                true -> redirect(Host, AnyId, Lang, Context);
                false -> undefined
            end;
        [[C|_] = AnyId] when C >= $0, C =< $9 ->
            case z_utils:only_digits(AnyId) of
                true -> redirect(Host, AnyId, undefined, Context);
                false -> undefined
            end;
        [Rsc|_] ->
            old_anymeta_url(Host, Rsc, Context);
        [] ->
            undefined
    end.

%% @doc When importing several Anymeta sites into one Zotonic instance,
%%		we also wanna catch requests belonging to old host names.
observe_dispatch_host(#dispatch_host{host=Host, path=Path}, Context) ->
	KnownHosts = z_depcache:memo(
		fun() ->
			case z_db:table_exists(import_anymeta, Context) of
				true ->
					lists:map(
						fun({H}) -> z_convert:to_list(H) end,
						z_db:q("select distinct host from import_anymeta;", Context)
					);
				false ->
					[]
			end
		end,
		anymeta_dispatch_hosts,
		Context
	),
	case lists:member(Host, KnownHosts) of
		true ->
			% Path is not rewritten at this point, so make sure it will be.
			Parts = string:tokens(Path, "/"),
			case Parts of
				["id"|Rest] ->
					NewPath = string:join(lists:append([["anymetaid"], Rest]), "/"),
					observe_dispatch(#dispatch{host=Host, path=NewPath}, Context);
				_ ->
					observe_dispatch(#dispatch{host=Host, path=Path}, Context)
			end;
		false ->
			undefined
	end.

old_anymeta_url(Host, Rsc, Context) ->
    case filename:extension(Rsc) of
        ".html" ->
            case string:tokens(filename:rootname(Rsc), "-") of
                [_Kind,AnyId,[_,_] = Lang] ->
                    redirect(Host, AnyId, Lang, Context);
                [_Kind,AnyId] ->
                    redirect(Host, AnyId, undefined, Context);
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

redirect(Host, AnyId, Lang, Context) ->
    case any_to_rsc_id(Host, AnyId, Context) of
        undefined ->
            undefined;
        RscId when is_integer(RscId) ->
            redirect_rsc(RscId, Lang, Context)
    end.

any_to_rsc_id(Host, AnyId, Context) ->
    z_depcache:memo(
            fun() ->
                case z_db:q1("select rsc_id 
                              from import_anymeta
                              where host = $1
                                and anymeta_id = $2
                              limit 1",
                             [z_convert:to_list(Host), z_convert:to_integer(AnyId)],
                             Context)
                of
                    undefined -> 
                        % Fallback for any host (hostnames could have changed after the migration)
                        z_db:q1("select rsc_id 
                                 from import_anymeta
                                 where anymeta_id = $1
                                 limit 1",
                                [z_convert:to_integer(AnyId)],
                                Context);
                    RscId ->
                        RscId
                end
            end,
            {anymeta_dispatch_lookup, Host, AnyId},
            Context).


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
            {ok, #dispatch_redirect{
                location=URL,
				is_permanent=true
			}}
    end.

map_language("jp") -> "ja";
map_language(<<"jp">>) -> <<"ja">>;
map_language(Code) -> Code.
