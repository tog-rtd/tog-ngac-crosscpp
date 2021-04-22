% Declarative Policy Language

:- module(dpl,[
		policy/2, element/2, assign/3, associate/4, cond/3, conditions/2,
	        gg_policy/1, gg_element/2, gg_associate/4, gg_gateway/2,
	        cc_policy/1, cc_element/2, cc_associate/4, cc_assign/3,
	        cc_external_attribute/2, cc_local_cloud_gateway/3,
		policy_class/2,
		load_decl_policy/2, load_decl_policy_immediate/2, save_decl_policy/2,
		object_oattribute/2, object_oattribute_nd/2, object/2, object/8,
		user_uattribute/2, user/2,
	        decl2imp/2, imp2decl/3,
		cmdTerms2policy/2,
	        get_id_operation_set/3
	       ]).

:- use_module(dpl_conditions).
:- use_module(policies).
:- use_module(pio).
:- use_module(param).
:- use_module(epp_cpa).

% The lightweight policy model
%
% Cache of loaded policies as asserted clauses
% policy(PolicyName, PolicyRoot)
%
% element(PolicyName:PolicyRoot, Element)
%   where Element is: user(_), user_attribute(_)
%     object(_), object_attribute(_), policy_class(_),
%     opset(OpSetName, OpList), operation(OpName, OpInfo), operation(OpName),
%     composed_policy(P1,P2,Pcomposed),
%     connector('pm') (is an element of every policy)
%     assign(PolicyName:PolicyRoot, PolicyElement1, PolicyElement2)
%     associate(PolicyName:PolicyRoot, UserAttr, OpSetName, ObjectAttr)
%     cond(Condition,Element)
%     conditions(ConditionPreds)
%     external_attribute(_)

:- dynamic policy/2, element/2, assign/3, associate/4, cond/3, conditions/2.
:- dynamic gg_policy/1, gg_element/2, gg_associate/4, gg_gateway/2.
:- dynamic cc_policy/1, cc_element/2, cc_associate/4, cc_assign/3.
:- dynamic cc_external_attribute/2, cc_local_cloud_gateway/3.

% policy(PolicyName, PolicyClass)
%

policy_elements([user,user_attribute,object,object_attribute,policy_class,
		 operation,opset,composed_policy,assign,associate,connector,
		 cond,conditions,external_attribute]).

policy_elements_args([user(_),user_attribute(_),
		      object(_),object(_,_,_,_,_,_,_),
		      object_attribute(_),policy_class(_),operation(_),operation(_,_),
		      opset(_,_),composed_policy(_,_,_),assign(_,_),associate(_,_,_),
		      connector(_),cond(_,_),conditions(_),external_attribute(_)]).

conditional_policy_elements_args([assign(_,_),associate(_,_,_)]).

:- dynamic dpl_initialized/1.

dpl_initialized(false).

init:- param:initialized(true), !. % already initialized
init :-
	forall( policies:policy(Pn,Pr,Pg), unpack_policy( policy(Pn,Pr,Pg) ) ),
	forall( policies:gg_policy(Pn,Pg), unpack_policy( gg_policy(Pn,Pg) ) ),
	(   gg_policy(GPname)
	->  param:setparam(current_gpolicy,GPname)
	;   true
	),
	forall( policies:cc_policy(Pn,Pg), unpack_policy( cc_policy(Pn,Pg) ) ),
	(   cc_policy(CPname)
	->  param:setparam(current_cpolicy,CPname)
	;   true
	),
	dpl_conditions:init,
	retractall( dpl_initialized(_) ), assert( dpl_initialized(true) ).

re_init :- un_init, init.

un_init :-
	clear_policy,
	retractall( dpl_initialized(_) ), assert( dpl_initialized(false) ).

clear_policy :-
	purge_policy(_), % should have same effect as following 3 lines
	%retractall(policy(_,_)), retractall(element(_,_)),
	%retractall(assign(_,_,_)), retractall(associate(_,_,_,_)),
	%retractall(cond(_,_,_)), retractall(conditions(_,_)),
	param:setparam(current_policy,none),
	true.

check_valid_conditional_element(Rule) :-
	conditional_policy_elements_args(CEA),
	memberchk(Rule,CEA).


decl2imp(Dfile,Ifile) :-
	load_decl_policy(Dfile,PolicyName),
	save_as_cmds(PolicyName,Ifile).

save_as_cmds(PolicyName,CmdFile) :-
	pio:policy_cmdstrs(PolicyName,CmdStrs),
	(   param:verbose(on)
	->  ui:display_listq(CmdStrs,1)
	;   true
	),
	pio:save_cmdstrs_to_file(CmdFile,CmdStrs).

imp2decl(_Ifile,_Policy,_Dfile) :-
	% TO DO
	true.

load_decl_policy(Pfile,PolicyName) :-
	pio:load_term(Pfile,PolicyTerm),
	load_decl_policy_common(PolicyTerm,PolicyName).

load_decl_policy_immediate(PolicyAtom,PolicyName) :-
	read_term_from_atom(PolicyAtom,PolicyTerm,[]),
	load_decl_policy_common(PolicyTerm,PolicyName).

load_decl_policy_common(PolicyTerm,PolicyName) :-
	PolicyTerm = policy(PolicyName,PolicyRoot,PolicyElements),
	atom(PolicyName), atom(PolicyRoot), is_list(PolicyElements), !,
	retractall(policies:policy(PolicyName,_,_)),
	assertz(policies:PolicyTerm),
	unpack_policy(PolicyTerm).

save_decl_policy(Pfile,PolicyTerm) :-
	PolicyTerm = policy(_PolicyName,_PolicyRoot,_PolicyGraph),
	pio:save_term(Pfile,PolicyTerm),
	true.

cmdTerms2policy(_CmdTerms,_Policy) :- true.
	% UNIMPLEMENTED compatibility with PM
%	cmdTerms_policyElts(CmdTerms,PolicyElements),
%	Policy = policy(PolicyName,PolicyRoot,PolicyElements),
%	true.

% cmdTerms_policyElts([],[]).
% cmdTerms_policyElts([Term|Terms],[Elt|Elts]) :-	true.

%

unpack_policy(policy(PolicyName,PolicyRoot,PolicyElements)) :-
	purge_policy(PolicyName),
	assertz( policy(PolicyName,PolicyRoot) ),
	unpack_policy_elements(PolicyName:PolicyRoot,PolicyElements), !,
	perform_static_policy_checks(PolicyName:PolicyRoot).

unpack_policy( gg_policy(GGpolicyName,GGpolicyElements) ) :-
	purge_ggpolicy(GGpolicyName),
	assertz( gg_policy(GGpolicyName) ),
	unpack_gpolicy_elements(GGpolicyName,GGpolicyElements), !,
	perform_static_gpolicy_checks(GGpolicyName).

unpack_policy( cc_policy(CCpolicyName,CCpolicyElements) ) :-
	purge_ccpolicy(CCpolicyName),
	assertz( cc_policy(CCpolicyName) ),
	unpack_cpolicy_elements(CCpolicyName,CCpolicyElements), !,
	perform_static_cpolicy_checks(CCpolicyName).

% note that PName below is P:PC
unpack_policy_elements(_,[]).
unpack_policy_elements(PName,[assign(I,A)|PolElts]) :- !,
	assertz( assign(PName,I,A) ),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[associate(I,M,A)|PolElts]) :- !,
	assertz( associate(PName,I,M,A) ),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[cond(Cond,Rules)|PolElts]) :- is_list(Rules), !,
	unpack_policy_elements_cond(PName,Rules,Cond),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[cond(Cond,Rule)|PolElts]) :- !,
	unpack_policy_element_cond(PName,Rule,Cond),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[conditions(Conditions)|PolElts]) :- is_list(Conditions), !,
	(   conditions(PName,_)
	->  % only accept one conditions declaration per policy
	    format('Only one conditions declaration permitted per policy~n')
	;   assertz( conditions(PName,Conditions) ),
	    request_conditions(Conditions)
	),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[PolElt|PolElts]) :-
	policy_elements_args(PolEltsArgs),
	memberchk(PolElt,PolEltsArgs), !,
	assertz( element(PName,PolElt) ),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[_,PolElts]) :- % skip unknown element
	unpack_policy_elements(PName,PolElts).

unpack_policy_elements_cond(_,[],_) :- !.
unpack_policy_elements_cond(PName,[PolElt|PolElts],Cond) :-
	unpack_policy_element_cond(PName,PolElt,Cond),
	unpack_policy_elements_cond(PName,PolElts,Cond).

unpack_policy_element_cond(_,cond(_,_),_) :- !.
	% skip a nested cond, maybe generate a diagnostic message here
unpack_policy_element_cond(PName,Element,Cond) :-
	check_valid_conditional_element(Element), !,
	unpack_policy_elements(PName,[Element]),
	assertz( cond(PName,Cond,Element) ).
unpack_policy_element_cond(_,_,_). % skip invalid conditional element


unpack_gpolicy_elements(_,[]).
unpack_gpolicy_elements(PName,[gateway(G)|PolElts]) :- !,
	assertz( gg_gateway(PName,G) ),
	unpack_gpolicy_elements(PName,PolElts).
unpack_gpolicy_elements(PName,[gg_associate(I,M,A)|PolElts]) :- !,
	assertz( gg_associate(PName,I,M,A) ),
	unpack_gpolicy_elements(PName,PolElts).
unpack_gpolicy_elements(PName,[_,PolElts]) :- % skip unknown element
	unpack_gpolicy_elements(PName,PolElts).


unpack_cpolicy_elements(_,[]).
unpack_cpolicy_elements(PName,[cc_assign(I,A)|PolElts]) :- !,
	assertz( cc_assign(PName,I,A) ),
	unpack_cpolicy_elements(PName,PolElts).
unpack_cpolicy_elements(PName,[cc_associate(I,M,A)|PolElts]) :- !,
	assertz( cc_associate(PName,I,M,A) ),
	unpack_cpolicy_elements(PName,PolElts).
unpack_cpolicy_elements(PName,[external_attribute(A)|PolElts]) :- !,
	assertz( cc_external_attribute(PName,A) ),
	unpack_cpolicy_elements(PName,PolElts).
unpack_cpolicy_elements(PName,[local_cloud_gateway(LC,G)|PolElts]) :- !,
	assertz( cc_local_cloud_gateway(PName,LC,G) ),
	unpack_cpolicy_elements(PName,PolElts).
unpack_cpolicy_elements(PName,[_,PolElts]) :- % skip unknown element
	unpack_cpolicy_elements(PName,PolElts).



purge_policy(PolicyName) :-
%	retractall(policy(PolicyName:_,_)),
	retractall(policy(PolicyName,_)),
	retractall(element(PolicyName:_,_)),
	retractall(assign(PolicyName:_,_,_)),
	retractall(associate(PolicyName:_,_,_,_)),
	retractall(cond(PolicyName:_,_,_)),
	retractall(conditions(PolicyName:_,_)).

purge_ggpolicy(PolicyName) :-
	retractall(gg_policy(PolicyName)),
	retractall(gg_gateway(PolicyName,_)),
	retractall(gg_associate(PolicyName,_,_,_)).

purge_ccpolicy(PolicyName) :-
	retractall(cc_policy(PolicyName)),
	retractall(cc_assign(PolicyName,_,_)),
	retractall(cc_associate(PolicyName,_,_,_)),
	retractall(cc_external_attribute(PolicyName,_)),
	retractall(cc_local_cloud_gateway(PolicyName,_,_)).

perform_static_policy_checks(PName) :-
	% check that conditions used are declared
	(   perform_condition_check(PName)
	->  true
	;   format('Condition check for policy ~q failed.~n',PName)
	),
	% check that operations used are valid for the object class
	perform_object_class_check(PName),
	% check that assignment arguments are defined
	perform_assignments_check(PName),
	% check that association arguments are defined
	perform_associations_check(PName),
	% check that the graph is connected
	perform_connectedness_check(PName),
	% check that given policy root occurs as a PC in the policy
	true.

perform_static_gpolicy_checks(_).

perform_static_cpolicy_checks(_).


perform_condition_check(PName) :-
	conditions(PName,DeclaredConditions), !,
	% could be made more precise
	% currently ignores the number and type of arguments to the declared and used predicates
	findall( Pred, (cond(PName,Condition,_), functor(Condition,Pred,_)), UsedPreds),
	forall( member(P,UsedPreds),
		( condition_predicate_check(P,DeclaredConditions); built_in_binary_relation_name(P) )
	      ).
perform_condition_check(_).

condition_predicate_check(PredName,DeclaredPredicates) :-
	member(C,DeclaredPredicates), functor(C,PredName,_).

perform_object_class_check(_PName).
% check that operations are defined and not used in places they shouldn't be used

perform_assignments_check(_PName).
% check that the arguments of an assignment are defined and compatible

perform_associations_check(_PName).
% check that the arguments of an association are defined and compatible

perform_connectedness_check(_PName).
% check that the graph does not have disconnected components

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
object_oattribute(P,O) :- var(O), !, object_oattribute_nd(P,O).

object_oattribute(P,O) :- element(P,object(O)), !.
object_oattribute(P,O) :- element(P,object(O,_,_,_,_,_,_)), !.
object_oattribute(P,O) :- element(P,object_attribute(O)).

object_oattribute_nd(P,O) :- element(P,object(O)).
object_oattribute_nd(P,O) :- element(P,object(O,_,_,_,_,_,_)).
object_oattribute_nd(P,O) :- element(P,object_attribute(O)).

object(P,O) :- element(P,object(O)).
object(P,O) :- element(P,object(O,_,_,_,_,_,_)).

object(P,O,Oclass,Inh,Host,Path,BaseType,BaseName) :-
	element(P,object(O,Oclass,Inh,Host,Path,BaseType,BaseName)), !.
% default path for object element of the form object(<name>) :
object(P,O,file,no,localhost,Path,object_attribute,BaseName) :-
	element(P,object(O)), assign(P,O,BaseName), atom_concat('FILES/',O,Path).

user_uattribute(P,U) :- user(P,U).
user_uattribute(P,U) :- element(P,user_attribute(U)).

user(P,U) :- element(P,user(U)).

policy_class(P,PC) :- element(P,policy_class(PC)).

get_id_operation_set(PolicyClass,OpSetID, OpSet) :-
	element(PolicyName:PolicyClass, policy_class(PolicyClass)),
	element(PolicyName:PolicyClass, opset(OpSetID,OpSet)).

