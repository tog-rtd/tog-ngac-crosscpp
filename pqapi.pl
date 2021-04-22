% POLICY QUERY API
:- module(pqapi, []).

:- use_module(audit,[audit_gen/2]).
:- use_module(param).
:- use_module(dpl).
:- use_module(pdp).
:- use_module(sessions).
:- use_module(domains).
:- use_module(jsonresp).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Policy Query API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(pqapi), root_apis(pqapi), []).
:- http_handler(root('pqapi/'), api_unimpl, [prefix]).
:- http_handler(root(pqapi/access), pqapi_access, [prefix]).
:- http_handler(root(pqapi/accessm), pqapi_accessm, [prefix]).
:- http_handler(root(pqapi/caccess), pqapi_caccess, [prefix]).
:- http_handler(root(pqapi/users), pqapi_users, [prefix]).
:- http_handler(root(pqapi/getobjinfo), pqapi_getobjinfo, [prefix]).
:- http_handler(root(pqapi/paramecho), pqapi_paramecho, [prefix]).

pqapi([access,accessm,caccess,users,getobjectinfo]). % POLICY QUERY API

% Global Policy Query API
%:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(gpqapi), root_apis(gpqapi), []).
:- http_handler(root('gpqapi/'), api_unimpl, [prefix]).
:- http_handler(root(gpqapi/gaccess), gpqapi_gaccess, [prefix]).
:- http_handler(root(gpqapi/ggetinfo), gpqapi_ggetinfo, [prefix]).

gpqapi([gaccess,ggetinfo]). % GLOBAL POLICY QUERY API


%
% Policy Query API
%

% JSON response structure
% {
%     "respStatus" : "statusType",
%     "respMessage" : "statusDesc",
%     "respBody" : "statusBody"
% }
%
% json_resp(RespStatus,RespMessage,RespBody,JrespTerm,JrespAtom)
%
% assignments to the JSON response structure for each API are given in
% the documentation

% access/3
pqapi_access(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[user(User,[atom]),
				 ar(AR,[atom]),
				 object(Object,[atom]),
				 cond(Cond,[atom,optional(true)])
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	access_response(User,AR,Object,Cond),
	!.
pqapi_access(_) :- audit_gen(policy_query, access(failure)).

% access/4 - access with a condition and its actual parameters
pqapi_caccess(Request) :- % added optional cond to access obsoleting this
	std_resp_prefix,
	catch(
	     http_parameters(Request,[user(User,[atom]),
				 ar(AR,[atom]),
				 object(Object,[atom]),
				 cond(ArgListAtom,[atom])
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	access_response(User,AR,Object,ArgListAtom), !.
pqapi_caccess(_) :- audit_gen(policy_query, caccess(failure)).

% access_response/3
access_response(User,AR,Object) :- param:current_policy(deny), !,
	access_deny(deny,User,AR,Object).

access_response(User,AR,Object) :- param:current_policy(grant), !,
	access_grant(grant,User,AR,Object).

access_response(User,AR,Object) :-
	param:current_policy(Policy),
	(   Policy == none
	->  std_resp_MS(failure,'no current policy','')
	;   (   access_check(Policy,(User,AR,Object))
	    ->  access_grant(Policy,User,AR,Object)
	    ;   access_deny(Policy,User,AR,Object)
	    )
	).

% access_response/4
access_response(User,AR,Object,A) :- var(A), !, access_response(User,AR,Object).
access_response(User,AR,Object,_CondArgAtom) :- param:current_policy(deny), !,
	access_deny(deny,User,AR,Object).

access_response(User,AR,Object,_CondArgAtom) :- param:current_policy(grant), !,
	access_grant(grant,User,AR,Object).

access_response(User,AR,Object,CondArgAtom) :-
        read_term_from_atom(CondArgAtom,CondArg,[]),
	(   compound(CondArg) ; atom(CondArg) ; is_list(CondArg) ), !,
	param:current_policy(Policy),
	(   Policy == none
	->  std_resp_MS(failure,'no current policy','')
	;   (   access_check(Policy,(User,AR,Object),CondArg)
	    ->  access_grant(Policy,User,AR,Object)
	    ;   access_deny(Policy,User,AR,Object)
	    )
	).

access_grant(Policy,UserOrSession,AR,Object) :-
	( sessions:is_session(UserOrSession,U), User = session(U) ; User = UserOrSession ),
	% audit record will show session(<user>) if session invocation, otherwise just <user>
	audit_gen(policy_query, access_granted(Policy,(User,AR,Object))),
	param:grant_resp(Grant),
	std_resp_M(success,Grant,(User,AR,Object)). % for backward compatibility respond w/grant only

access_deny(Policy,UserOrSession,AR,Object) :-
	( sessions:is_session(UserOrSession,U), User = session(U) ; User = UserOrSession ),
	% audit record will show session(<user>) if session invocation, otherwise just <user>
	audit_gen(policy_query, access_denied(Policy,(User,AR,Object))),
	param:deny_resp(Deny),
	std_resp_M(success,Deny,(User,AR,Object)). % for backward compatibility respond w/deny only

% accessm
% pqapi_accessm was separately implemented after pqapi_access
% could potentially be merged with pqapi_access by rewriting both
%
% Currently the policy 'none' causes a failure response but by deleting
% or commenting-out the first clause of accessm/1 it can be changed into
% a success response with a return vector of all 'deny' results
%
pqapi_accessm(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[access_queries(QueryListAtom,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	accessm(QueryListAtom), !.
pqapi_accessm(_) :- audit_gen(policy_query, accessm(failure)).

accessm(_) :- param:current_policy(none), !, % could delete this clause and go with all deny response vector
	std_resp_MS(failure,'no current policy',''), !, fail.
accessm(QueryListAtom) :-
        read_term_from_atom(QueryListAtom,Queries,[]), is_list(Queries), !,
	param:current_policy(Policy),
	accessm_results(Policy,Queries,Results),
	std_resp_BS(success,Queries,Results),
	audit_gen(policy_query, accessm('multiple queries complete',success)).
accessm(QueryListAtom) :-
	std_resp_BS(failure,'malformed query list',QueryListAtom),
	audit_gen(policy_query, accessm('malformed query list',failure)).

accessm_results(_,[],[]) :- !.
% deny, none, and grant all bypass the real access check in accessm_result
accessm_results(deny,Q,R) :- !, same_length(Q,R), maplist(param:deny_resp,R).
accessm_results(none,Q,R) :- !, same_length(Q,R), maplist(param:deny_resp,R). % for calls that bypass accessm
accessm_results(grant,Q,R) :- !, same_length(Q,R), maplist(param:grant_resp,R).
accessm_results(Policy,[Query|Queries],[Result|Results]) :-
	accessm_result(Policy,Query,Result),
	accessm_results(Policy,Queries,Results).

%accessm_result(P,(U,R,O),Result) :- var(Result), % nonvar is reported as malformed query
%	(   access_check(P,(U,R,O))
%	->  param:grant_resp(Result)
%	;   param:deny_resp(Result)
%	), !.
%accessm_result(P,(U,R,O,C),Result) :- var(Result),
%	(	caccess_check(P,(U,R,O),C)
%	->	param:grant_resp(Result)
%	;	param:deny_resp(Result)
%	), !.
% accessm_result(P,Q,Result) :- var(Result), % nonvar is reported as malformed query
%	check_mquery(Q,U,R,O,C),
%	(   caccess_check(P,(U,R,O),C)
%	->  param:grant_resp(Result)
%	;   param:deny_resp(Result)
%	), !.
accessm_result(P,Q,Result) :- var(Result), % nonvar is reported as malformed query
	Q = (U,R,O), \+compound(O), !,
	(   access_check(P,(U,R,O))
	->  param:grant_resp(Result)
	;   param:deny_resp(Result)
	).
accessm_result(P,Q,Result) :- var(Result), % nonvar is reported as malformed query
	Q = (U,R,O,C), ( compound(C) ; C==true ), !,
	(   access_check(P,(U,R,O),C)
	->  param:grant_resp(Result)
	;   param:deny_resp(Result)
	).
accessm_result(_,_,'malformed query').

pqapi_users(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[object(O,[atom]),
				     %mode(M,[atom,optional(true)]),
				     ar(AR,[atom,optional(true)]),
				     cond(CondAtom,[atom,optional(true)])
				    ]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	AR = M,
	users(O,M,CondAtom).
pqapi_users(_) :- audit_gen(policy_query, users(failure)).

users(O,M,C) :- var(C), !, users(O,M). % no condition supplied
users(O,M,C) :- C==true, !, users(O,M). % the condition is 'true'
users(O,M,C) :- string(C), !, atom_string(Catom,C), users(O,M,Catom).
%users(O,M,C) :- string(C), !,
%	read_term_from_chars(C,Cond,[]),
%	param:current_policy(P),
%	(   pdp:aua_users(P,O,_PC,M,Cond,Users)
%	->  std_resp_BS(success, users(O), Users)
%	;   std_resp_MS(failure, users, O)
%	).
users(O,M,Catom) :- atom(Catom),
	read_term_from_atom(Catom,Cond,[]),
	param:current_policy(P),
	(   pdp:aua_users(P,O,_PC,M,Cond,Users)
	->  std_resp_BS(success, users(O), Users)
	;   std_resp_MS(failure, users(O), '')
	).
/*
users2(O,M,C) :- is_list(C), !, % condition var defs list?
	is_cond_var_list(C),
	param:current_policy(P),
	(   pdp:aua_users(P,O,_PC,M,C,Users)
	->  std_resp_BS(success, users, Users)
	;   std_resp_MS(failure, users, O)
	).
users2(O,M,C) :- (atom(C) ; compound(C)), !, % a condition predicate?
	is_cond_pred(C),
	param:current_policy(P),
	true.
*/

% users/2 there is no condition supplied
users(O,M) :- var(M), !, % no AR is specified
	param:current_policy(P),
	(   pdp:aua_users(P,O,_PC,Users)
	->  std_resp_BS(success, users, Users)
	;   std_resp_MS(failure, users, O)
	).

users(O,M) :- % an AR is specified
	param:current_policy(P),
	(   pdp:aua_users(P, (M,O), Users)
	->  std_resp_BS(success ,users, Users)
	;   std_resp_MS(failure, users, (O,M))
	).

is_cond_pred(CP) :- % CP =.. [_C|_Cargs],
	dpl_conditions:validate_condition_predicate(CP,_). % HERE more to do

is_cond_var_list(C) :- dpl_conditions:is_cond_var_def_list(C).
% is_cond_var_list([]).
% is_cond_var_list([C|Cs]) :- cond_var_list_item(C),
% is_cond_var_list(Cs).
% cond_var_list_item(CVar=Val) :- atom(CVar), atom(Val). % HERE more to do

check_mquery(Q,U,R,O,true) :- Q = (U,R,O), \+compound(O), !.
check_mquery(Q,U,R,O,C) :- Q = (U,R,O,C), (compound(C);C==true), !.

% getobjinfo
pqapi_getobjinfo(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[object(O,[atom])]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	param:current_policy(P),
	getobjinfo(P,O),
        audit_gen(policy_query, getojfinfo(P,O)), !.
pqapi_getobjinfo(_) :- audit_gen(policy_query, getobjinfo(failure)).

getobjinfo(P,O) :-
	(   ( dpl:policy(P,Pr), dpl:object(P:Pr,O) )
	->  ( dpl:object(P:Pr,O,Oclass,Inh,Host,Path,BaseType,BaseName)
	    ; Oclass='', Inh='', Host='', Path='', BaseType='', BaseName=''
	    ),
	    std_resp_BS(success,objectinfo,objectinfo(O,Oclass,Inh,Host,Path,BaseType,BaseName))
	;   std_resp_MS(failure,'unknown policy or object',(P,O)),
	    audit_gen(policy_admin, getobjinfo(P,O,failure))
	).

pqapi_paramecho(Request) :- % for testing
	std_resp_prefix,
	format('Request=~q~n',[Request]),
	catch(
	    http_parameters(Request,[],[form_data(Params)]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	std_resp_BS(success,paramecho,Params),
	audit_gen(policy_query, paramecho(Params,success)), !.
pqapi_paramecho(_) :- audit_gen(policy_query, paramecho(failure)).


%
% Global Policy Query API
%
%     gaccess(G1,CommOp,G2) may G1 perform comm operation CommOp to G2 ?
%
%     ggetinfo( )
%

% gaccess
gpqapi_gaccess(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[src(Src,[atom]),
				 op(Op,[atom]),
				 dst(Dst,[atom])
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	gaccess_response(Src,Op,Dst), !.
gpqapi_gaccess(_) :- audit_gen(policy_query, gaccess(failure)).

gaccess_response(Src,Op,Dst) :- param:current_policy(deny), !,
	gaccess_deny(deny,Src,Op,Dst).

gaccess_response(Src,Op,Dst) :- param:current_policy(grant), !,
	gaccess_grant(grant,Src,Op,Dst).

gaccess_response(Src,Op,Dst) :-
	param:current_policy(LocalPolicy),
	(   LocalPolicy == none
	->  std_resp_MS(failure,'no current local policy',''),
	    !, fail
	;   true
	),
	param:current_gpolicy(GlobalPolicy),
	(   GlobalPolicy == none
	->  std_resp_MS(failure,'no current global policy',''),
            !, fail
	;
	    (   gaccess_check(LocalPolicy,GlobalPolicy,(Src,Op,Dst))
	    ->  gaccess_grant(LocalPolicy,Src,Op,Dst)
	    ;   gaccess_deny(LocalPolicy,Src,Op,Dst)
	    )
	).

gaccess_grant(Policy,Src,Op,Dst) :-
	% may have to handle sessions here
	audit_gen(gpolicy_query, gaccess_granted(Policy,(Src,Op,Dst))),
	std_resp_M(success,grant,(Src,Op,Dst)).

gaccess_deny(Policy,Src,Op,Dst) :-
	% may have to handle sessions here
	audit_gen(gpolicy_query, gaccess_denies(Policy,(Src,Op,Dst))),
	std_resp_M(success,deny,(Src,Op,Dst)).

% ggetinfo
gpqapi_ggetinfo(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	% not yet implemented
	audit_gen(gpolicy_query, ggetinfo(unimplemented)),
	std_resp_MS(failure,'ggetinfo unimplemented',''), !.
gpqapi_ggetinfo(_) :- audit_gen(gpolicy_query, ggetinfo(failure)).


%
% JSON response structure
% {
%     "respStatus" : "statusType",
%     "respMessage" : "statusDesc",
%     "respBody" : "statusBody"
% }
%
% json_resp(RespStatus,RespMessage,RespBody)
%

std_resp_prefix :-
	(   param:jsonresp(on)
	->  format('Content-type: application/json~n~n')
	;   format('Content-type: text/plain~n~n')
	).

std_resp_MS(Status, M, B) :-
	(   param:jsonresp(on)
	->  json_resp(Status, M, B)
	;   writeln(M), writeln(Status)
	).

std_resp_BS(Status, M, B) :-
	(   param:jsonresp(on)
	->  json_resp(Status, M, B)
	;   writeln(B), writeln(Status)
	).

std_resp_M(Status, M, B) :-
	(   param:jsonresp(on)
	->  json_resp(Status, M, B)
	;   writeln(M)
	).

%
%
%

api_unimpl(_) :-
	std_resp_prefix,
	format('Unimplemented API~n').

root_apis(Kind,_) :- std_resp_prefix, list_apis(Kind), !.
root_apis(_,_).

list_apis(Kind) :-
	format('Valid ~a paths:~n',[Kind]),
	G=..[Kind,APIs], call(G),
	foreach( member(A,APIs), writeln(A)).

use_pqapi(_) :-
	std_resp_prefix,
	format('Use (g)pqapi as root for policy query APIs~n'),
	list_apis(pqapi), list_apis(gpqapi).

use_paapi(_) :-
	std_resp_prefix,
	format('Use (g)paapi as root for policy administration APIs~n'),
	list_apis(paapi), list_apis(gpaapi).

use_valid_api(_) :-
	format('Use (g)paapi for policy admin, (g)pqapi for policy query~n').

