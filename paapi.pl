% POLICY ADMINISTRATION API
:- module(paapi, []).

:- use_module(audit,[audit_gen/2]).
:- use_module(param).
:- use_module(dpl).
%:- use_module(dpl_conditions).
:- use_module(sessions).
:- use_module(pap).
:- use_module(jsonresp).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Policy Administration Web API
:- http_handler(root(paapi), root_apis(paapi), []).
:- http_handler(root('paapi/'), api_unimpl, [prefix]).
:- http_handler(root(paapi/add), paapi_add, [prefix]).
:- http_handler(root(paapi/addm), paapi_addm, [prefix]).
:- http_handler(root(paapi/delete), paapi_delete, [prefix]).
:- http_handler(root(paapi/deletem), paapi_deletem, [prefix]).
:- http_handler(root(paapi/getpol), paapi_getpol, [prefix]).
:- http_handler(root(paapi/setpol), paapi_setpol, [prefix]).
:- http_handler(root(paapi/combinepol), paapi_combinepol, [prefix]).
% :- http_handler(root(paapi/importpol), paapi_loadpol, [prefix]). % deprecated
:- http_handler(root(paapi/load), paapi_loadpol, [prefix]).
:- http_handler(root(paapi/loadi), paapi_loadpoli, [prefix]).
:- http_handler(root(paapi/readpol), paapi_readpol, [prefix]).
:- http_handler(root(paapi/purgepol), paapi_unloadpol, [prefix]).
:- http_handler(root(paapi/unload), paapi_unloadpol, [prefix]).
:- http_handler(root(paapi/loadcondi), paapi_loadcondi, [prefix]).
:- http_handler(root(paapi/unloadcondi), paapi_unloadcondi, [prefix]).
:- http_handler(root(paapi/readcond), paapi_readcond, [prefix]).
:- http_handler(root(paapi/reset), paapi_reset, [prefix]).
:- http_handler(root(paapi/resetcond), paapi_resetcond, [prefix]).
:- http_handler(root(paapi/initsession), paapi_initsession, [prefix]).
:- http_handler(root(paapi/endsession), paapi_endsession, [prefix]).

% Global Policy Admin API
:- http_handler(root(gpaapi), root_apis(gpaapi), []).
:- http_handler(root('gpaapi/'), api_unimpl, [prefix]).
:- http_handler(root(gpaapi/getgpol), gpaapi_getgpol, [prefix]).
:- http_handler(root(gpaapi/setgpol), gpaapi_setgpol, [prefix]).

% POLICY ADMIN APIs
paapi([add,delete,getpol,setpol,combinepol,load,loadi,readpol,importpol,purgepol,unload,
       loadcondi,unloadcondi,readcond,resetcond,reset,initsession,endsession]).

% GLOBAL POLICY ADMIN APIs
gpaapi([getgpol,setgpol]).

%
% Policy Administration API
%

% add
paapi_add(Request) :-
	std_resp_prefix,
	parse_add_delete_arguments(Request, Policy, PElement, Token),
	(   authenticate(Token)
	->  add(Policy,PElement)
	;   true
	).
paapi_add(_) :- audit_gen(policy_admin, add(failure)).

add(Policy,PElement) :-
	(   add_policy_element(Policy,PElement)
	->  std_resp_MS(success,'element added',PElement),
	    audit_gen(policy_admin, add(Policy, PElement, success))
	;   std_resp_MS(failure,'error adding element',PElement),
	    audit_gen(policy_admin, add(Policy, PElement, failure))
	).

% delete
paapi_delete(Request) :-
	std_resp_prefix,
	parse_add_delete_arguments(Request, Policy, PElement, Token),
	(   authenticate(Token)
	->  delete(Policy,PElement), !
	;   true
	).
paapi_delete(_) :- audit_gen(policy_admin, delete(failure)).

delete(Policy,PElement) :-
	(   delete_policy_element(Policy,PElement)
	->  std_resp_MS(success,'element deleted',PElement),
	    audit_gen(policy_admin, delete(Policy, PElement, success))
	;   std_resp_MS(failure,'error deleting element',PElement),
	    audit_gen(policy_admin, delete(Policy, PElement, failure))
	).

parse_add_delete_arguments(Request, Policy, PElement, Token) :-
	catch(
	    http_parameters(Request,[policy(Policy,[atom]),
				     % accept either of the following for backward compat
				     policy_element(P_E,[atom,optional(true)]),
				     policyelement(PE,[atom,optional(true)]),
				     token(Token,[atom])
				   ]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   ( ( var(P_E), var(PE) ) ; ( nonvar(P_E), nonvar(PE) ) )
	->  std_resp_MS(failure,'error parsing request arguments',PElement),
	    !, fail
	;   P_E = PE
	),
	(
	    (
		read_term_from_atom(PE,PElement,[]),
		ground(PElement), PElement =.. [PEf|_PEargs],
		permitted_add_delete_policy_elements(Permitted),
		memberchk(PEf,Permitted)
	    )
	;
	    std_resp_MS(failure,'error in argument',''),
	    !, fail
	), !.

% addm
paapi_addm(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(Policy,[atom]),
				     policy_elements(EltListAtom,[atom]),
				     name(Name,[atom,optional(true)]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  addm(Policy,EltListAtom,Name), !
	;   true
	).
paapi_addm(_) :- audit_gen(policy_admin, addm(failure)).

addm(Policy,EltListAtom,_Name) :-
        ( ( read_term_from_atom(EltListAtom,EltList,[]), is_list(EltList),
	      add_policy_elements(Policy,EltList) )
          ->  std_resp_MS(success,'elements added',EltList),
              audit_gen(policy_admin, addm(Policy, 'elements added'))
	  ;   std_resp_MS(failure,'error adding elements',EltListAtom),
              audit_gen(policy_admin, addm(Policy, 'error adding elements'))
	).

% deletem
paapi_deletem(Request) :-
	std_resp_prefix,
	catch(
	    (	http_parameters(Request,[policy(Policy,[atom]),
				     policy_elements(EltListAtom,[atom,optional(true)]),
				     name(Name,[atom,optional(true)]),
				     token(Token,[atom])]),
	        ( var(EltListAtom) ; var(Name) ) % one must be specified but not both
	    ),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  deletem(Policy,EltListAtom,Name), !
	;   true
	).
paapi_deletem(_) :- audit_gen(policy_admin, deletem(failure)).

deletem(_Policy,EltListAtom,Name) :- ground(EltListAtom), ground(Name), !, fail.
deletem(Policy,EltListAtom,_Name) :- ground(EltListAtom), !,
        ( ( read_term_from_atom(EltListAtom,EltList,[]), is_list(EltList),
	      delete_policy_elements(Policy,EltList) )
          ->  std_resp_MS(success,'elements deleted',EltList),
              audit_gen(policy_admin, deletem(Policy, 'elements deleted'))
	  ;   std_resp_MS(failure,'error deleting elements',EltListAtom),
              audit_gen(policy_admin, deletem(Policy, 'error deleting elements'))
	).
deletem(_Policy,_EltListAtom,Name) :- ground(Name), !,
	fail. % named set deletem not yet implemented

% getpol
paapi_getpol(Request) :- % set current policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  getpol, !
	;   true
	).
paapi_getpol(_) :- audit_gen(policy_admin, getpol(failure)).

getpol :-
	get_current_policy(P),
	std_resp_BS(success,'current policy',P),
	audit_gen(policy_admin, getpol(success)).

% setpol
paapi_setpol(Request) :- % set current policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  setpol(P), !
	;   true
	).
paapi_setpol(_) :- audit_gen(policy_admin, setpol(failure)).

setpol(P) :-
	(   ( dpl:policy(P,_); P==all; P==allnc ; P==grant; P==deny; P==none )
	->  set_current_policy(P),
	    std_resp_BS(success,'policy set',P),
	    audit_gen(policy_admin, setpol(P,success))
	;   std_resp_MS(failure,'unknown policy',P),
	    audit_gen(policy_admin, setpol(P,failure))
	).

% loadpol
paapi_loadpol(Request) :- % load policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policyfile(Pfile,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  loadpol(Pfile), !
	;   true
	).
paapi_loadpol(_) :- audit_gen(policy_admin, loadpol(failure)).


loadpol(Pfile) :-
	(   ( exists_file(Pfile), load_policy(Pfile,PolicyName) )
	->  % TODO add check for: all, none, grant, deny
	    std_resp_BS(success,'policy loaded',PolicyName),
	    audit_gen(policy_admin, load(Pfile,PolicyName,success))
	;   std_resp_MS(failure,'file or load error',Pfile),
	    audit_gen(policy_admin, load(Pfile,failure))
	).

% loadi
paapi_loadpoli(Request) :- % load policy immediate
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policyspec(Pspec,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  loadpoli(Pspec), !
	;   true
	).
paapi_loadpoli(_) :- audit_gen(policy_admin, loadpoli(failure)).

loadpoli(Pspec) :-
	(   ( ground(Pspec), load_policy_immediate(Pspec,PolicyName) )
	->  std_resp_BS(success,'policy loaded immediate',PolicyName),
	    audit_gen(policy_admin, loadi(Pspec,PolicyName,success))
	;   std_resp_MS(failure,'malformed policy or load error',Pspec),
	    audit_gen(policy_admin, loadi(Pspec,failure))
	).

% unloadpol
paapi_unloadpol(Request) :- % unload policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  unloadpol(P), !
	;   true
	).
paapi_unloadpol(_) :- audit_gen(policy_admin, unloadpol(failure)).


unloadpol(P) :-
	(   dpl:policy(P,_)
	->  unload_policy(P),
	    std_resp_MS(success,'policy unloaded',P),
	    audit_gen(policy_admin, unloadpol(P,success))
	;   std_resp_MS(failure,'unknown policy',P),
	    audit_gen(policy_admin, unloadpol(P,'unknown policy failure'))
	).

% readpol
paapi_readpol(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom,default(current_policy)]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  readpol(P), !
	;   true
	).
paapi_readpol(_) :- audit_gen(policy_admin, readpol(failure)).

readpol(P) :-
	(   ( P==current_policy, param:current_policy(PN), PN\==none ; policy(P,_), PN=P )
	->  policies:policy(PN,_PC,_PE), % PTerm = policy(PN,PC,PE),
	    with_output_to( atom(PAtom), pio:display_policy(PN) ),
	    std_resp_BS(success,'read policy',PAtom)
	;   std_resp_MS(failure,'unknown policy',P),
	    audit_gen(policy_admin, readpol(P,failure))
	).

% combinepol
paapi_combinepol(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy1(P1,[atom]),
				     policy2(P2,[atom]),
				     combined(Pc,[atom]),
				     token(Token,[atom])
				    ]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  combinepol(P1,P2,Pc), !
	;   true
	).
paapi_combinepol(_) :- audit_gen(policy_admin, combinepol(failure)).

combinepol(P1,P2,Pc) :-
	(   pap:compose_policies(P1,P2,Pc)
	->  std_resp_BS(success,'policies combined',Pc),
	    audit_gen(policy_admin, combinepol(P1,P2,Pc,success))
	;   std_resp_MS(failure,'error combining policies',''),
	    audit_gen(policy_admin, combinepol(P1,P2,Pc,failure))
	).

% loadcondi - load condition variables and predicates
paapi_loadcondi(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[cond_name(Cname,[atom,default(dynamic)]),
				     cond_elements(EltListAtom,[atom]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  loadcondi(Cname,EltListAtom), !
	;   true
	).
paapi_loadcondi(_) :- audit_gen(policy_admin, loadcondi(failure)).

loadcondi(Cname,EltListAtom) :-
        ( ( read_term_from_atom(EltListAtom,EltList,[]), is_list(EltList),
	      dynamic_add_cond_elements(Cname,EltList) )
          ->  std_resp_MS(success,'cond elements added',Cname),
              audit_gen(policy_admin, loadcondi(Cname, 'cond elements added'))
	  ;   std_resp_MS(failure,'error adding cond elements',EltListAtom),
              audit_gen(policy_admin, loadcondi(Cname, 'error adding elements'))
	).

% unloadcondi
paapi_unloadcondi(Request) :-
	std_resp_prefix,
	catch(
	    (	http_parameters(Request,[cond_name(Cname,[atom,default(user_defined)]),
				     cond_elements(EltListAtom,[atom,optional(true)]),
				     token(Token,[atom])]),
		( ground(Cname) ; ground(EltListAtom) ) % at least one must be specified
	    ),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  unloadcondi(Cname,EltListAtom), !
	;   true
	).
paapi_unloadcondi(_) :- audit_gen(policy_admin, unloadcondi(failure)).

unloadcondi(_Cname,EltListAtom) :- var(EltListAtom), !, fail. % for now
unloadcondi(Cname,EltListAtom) :- % Cname is either 'user_defined' or specified name
        ( ( read_term_from_atom(EltListAtom,EltList,[]), is_list(EltList),
	      dynamic_delete_cond_elements(Cname,EltList) )
          ->  std_resp_MS(success,'cond elements deleted',EltList),
              audit_gen(policy_admin, unloadcondi(Cname, 'cond elements unloaded'))
	  ;   std_resp_MS(failure,'error unloading cond elements',EltListAtom),
              audit_gen(policy_admin, unloadcondi(Cname, 'error unloading cond elements'))
	).

% readcond
paapi_readcond(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[cond_name(CN,[atom,default(dynamic)]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  readcond(CN), !
	;   true
	).
paapi_readcond(_) :- audit_gen(policy_admin, readcond(failure)).

readcond(CN) :-
	(   dpl_conditions:is_cond_name(CN)
	->
	    with_output_to( atom(CAtom), pio:display_conditions(CN) ),
	    std_resp_BS(success,'read conditions',CAtom)
	;   std_resp_MS(failure,'unknown condition name',CN),
	    audit_gen(policy_admin, readcond(CN,failure))
	).

% reset
paapi_reset(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[domain(Dom,[atom,default(conditions)]),
				     name(Name,[atom,default(dynamic)]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  reset(Dom,Name), !
	;   true
	).
paapi_reset(_) :- audit_gen(policy_admin, reset(failure)).

reset(conditions,CN) :- !, % conditions domain
	(   dpl_conditions:is_cond_name(CN)
	->
	    preset(conditions,CN),
	    std_resp_BS(success,'reset conditions',CN)
	;   std_resp_MS(failure,'unknown condition name',CN),
	    audit_gen(policy_admin, reset(cond,CN,failure))
	).
reset(policies,_PN) :- !. % policies domain - noop for now
reset(_,_). % ignore any other domain for now

% resetcond - short-cut for conditions
%
% cond_name is the name of a condition set
% if cond_name is not supplied it defaults to 'dynamic'
% which will cause all dynamically loaded conditions to be unloaded
%
paapi_resetcond(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[cond_name(CN,[atom,default(all)]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  reset(conditions,CN), !
	;   true
	).
paapi_resetcond(_) :- audit_gen(policy_admin, resetcond(failure)).

% initsession
paapi_initsession(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[session(S,[atom]),
				    user(U,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  initsession(S,U), !
	;   true
	).
paapi_initsession(_) :- audit_gen(policy_admin, initsession(failure)).

initsession(S,U) :-
	(   \+is_session(S,_)
	->  init_session(S,U),
	    std_resp_BS(success,'session initialized',S),
	    audit_gen(policy_admin, initsession(S,U,success))
	;   std_resp_MS(failure,'session already registered',S),
	    audit_gen(policy_admin, initsession(S,U,failure))
	).

% endsession
paapi_endsession(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[session(S,[atom]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  endsession(S), !
	;   true
	).
paapi_endsession(_) :- audit_gen(policy_admin, endsession(failure)).

endsession(S) :-
	(   is_session(S,_)
	->  end_session(S),
	    std_resp_MS(success,'session ended',S),
	    audit_gen(policy_admin, endsession(S,success))
	;   std_resp_MS(failure,'session unknown',S),
	    audit_gen(policy_admin, endsession(S,failure))
	).

%
% GLOBAL POLICY ADMIN
%

gpaapi_getgpol(Request) :- % set current policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  getgpol, !
	;   true
	).
gpaapi_getgpol(_) :-audit_gen(policy_admin, getgpol(failure)).

getgpol :-
	get_current_gpolicy(GP),
	std_resp_BS(success,'current global policy',GP),
	audit_gen(policy_admin, getgpol(success)).

gpaapi_setgpol(Request) :- % set current policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(GP,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  setgpol(GP), !
	;   true
	).
gpaapi_setgpol(_) :- audit_gen(policy_admin, setgpol(failure)).

setgpol(GP) :-
	(   dpl:policy(GP,_)
	->  set_current_gpolicy(GP),
	    std_resp_BS(success,'global policy set',GP),
	    audit_gen(policy_admin, setgpol(GP,success))
	;   std_resp_MS(failure,'unknown global policy',GP),
	    audit_gen(policy_admin, setgpol(GP,failure))
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

%use_valid_api(_) :-
%	format('Use (g)paapi for policy admin, (g)pqapi for policy
%	query~n').

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

std_resp_S(Status, M, B) :-
	(   param:jsonresp(on)
	->  json_resp(Status, M, B)
	;   writeln(Status)
	).


authenticate(Token) :-
	(   authenticate_token(Token)
	->  true
	;   std_resp_M(failure,'authentication error',''),
	    audit_gen(policy_admin, 'authentication error'),
	    !, fail
	).

authenticate_token(Token) :- atom(Token), param:admin_token(Token), !.


read_term_from_atom_in_list([],[]).
read_term_from_atom_in_list([Elt|Elts],[TElt|TElts]) :-
	read_term_from_atom(Elt,TElt,[]),
	read_term_from_atom_in_list(Elts,TElts).

