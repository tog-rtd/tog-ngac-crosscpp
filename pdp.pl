% Isolated Policy Decision Point

:- module(pdp, [access_check/2, access_check/3]).

:- use_module([dpl,dpl_conditions]).

:- include('TEST/pdp_test').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% access_check/2 is the standard query triple (U,A,O) under policy P
% access_check/3 is a "condional query" with condition pred and params
%
access_check(P, (S,M,O)) :-
	atom(P), atom(S), atom(M), atom(O),
	(   sessions:is_session(S,U) ; U = S  ), !, atom(U),
	% report_event(access_check_event), % http or internal
	access_check1(P,(U,M,O)).

access_check1(all, (U,M,O)) :- !,
	param:all_composition(V),
	policy_dpc(all:V, (U,M,O)).
access_check1(P, (U,M,O)) :-
	policy(P,PC),
	policy_dpc(P:PC,(U,M,O)), !. % note policy_dpc/2

access_check(P, (S,M,O), CA) :-
	atom(P), atom(S), atom(M), atom(O), (is_list(CA);compound(CA);atom(CA)),
	(   sessions:is_session(S,U) ; U = S  ), !, atom(U),
	% report_event(access_check_event), % http or internal
	access_check1c(P,(U,M,O),CA).

access_check1c(all, (U,M,O), _CA) :- !, fail, % don't do 'all' for now
	param:all_composition(V),
	policy_dpc(all:V, (U,M,O)).
access_check1c(P, (U,M,O), CA) :-
	policy(P,PC),
	policy_dpc(P:PC,(U,M,O), CA), !. % note policy_dpc/3

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a derived privilege of the combined policy
% for all policy classes having E there is a solution for (U,AR,E)
% As it is policy_dpc cannot be used as an enumerator as could policy_dp
% This breaks los() and user_los() until they are correspondingly
% updated. Fortunately they are non-essential in the current usage.
%
% Policy composition algorithm variations for policy 'all' query a
% subset of the currently loaded policies based on one of the qualifying
% conditions:
%   variant #1 p_o all policies having the object O
%   variant #2 p_uo all policies having the user U and the object O
%   variant #3 pc_o all policy classes having the object O
%   variant #4 pc_uo all policy classes having the user U and object O
%
% These are based on how the policies to be consulted are qualified.
% All policies currently loaded are checked for qualification.
% The normal policy_dpc is invoked for all qualified policies.
%
% Depending on qualification of P and PC:
%   a policy qualifies if it governs O and U qualified_p_for_uo
%   a policy qualifies if it governs O qualified_p_for_o
%
%   a pc qualifies if it governs O and U qualified_pc_for_uo
%   a pc qualifies if it governs O qualified_pc_for_o
%

policy_dpc(all:p_o,(U,AR,O)) :- !,
	forall( (dpl:policy(P,Pr), qualified_p_for_o(P:Pr,O)),
		 policy_dpc(P:Pr,(U,AR,O)) ).
policy_dpc(all:p_uo,(U,AR,O)) :- !,
	forall( (dpl:policy(P,Pr), qualified_p_for_uo(P:Pr,U,O)),
		 policy_dpc(P:Pr,(U,AR,O)) ).

% these 4 'x' versions generate diagnostic output
policy_dpc(all:xp_o,(U,AR,O)) :- !,
	findall(P:Pr, (dpl:policy(P,Pr)), Policies),
	format('all policies: ~q~n',[Policies]),

	findall(P:Pr, (member(P:Pr,Policies),qualified_p_for_o(P:Pr,O)), QPolicies),
	format('qualifying policies: ~q~n',[QPolicies]),
	findall(R, (member(P:Pr,QPolicies), (policy_dpc(P:Pr,(U,AR,O))->R=grant;R=deny)), QPresults),
	format('Results: ~q~nOverall: ',[QPresults]),
	(   memberchk(deny,QPresults) -> writeln(deny) ; writeln(grant) ).
policy_dpc(all:xp_uo,(U,AR,O)) :- !,
	findall(P:Pr, (dpl:policy(P,Pr)), Policies),
	format('all policies: ~q~n',[Policies]),

	findall(P:Pr, (member(P:Pr,Policies),qualified_p_for_uo(P:Pr,U,O)), QPolicies),
	format('qualifying policies: ~q~n',[QPolicies]),
	findall(R, (member(P:Pr,QPolicies), (policy_dpc(P:Pr,(U,AR,O))->R=grant;R=deny)), QPresults),
	format('Results: ~q~nOverall: ',[QPresults]),
	(   memberchk(deny,QPresults) -> writeln(deny) ; writeln(grant) ).
policy_dpc(all:xpc_o,(U,AR,O)) :- !,
	forall( (dpl:policy(P,Pr),
		 findall(PC, policy_class(P:Pr,PC), PCs),
		 member(Pc,PCs), qualified_pc_for_o(P:Pr,Pc,O)),
		(format('policy ~q, policy classes ~q~n',[P,PCs]),
		 policy_dpc(P:Pr,(U,AR,O))) ).
policy_dpc(all:xpc_uo,(U,AR,O)) :- !,
	forall( (dpl:policy(P,Pr),
		 findall(PC, policy_class(P:Pr,PC), PCs),
		 member(Pc,PCs), qualified_pc_for_uo(P:Pr,Pc,U,O)),
		(format('policy ~q, policy classes ~q~n',[P,PCs]),
		 policy_dpc(P:Pr,(U,AR,O))) ).
% the core test - policy_dpc/2
policy_dpc(P:Pr,(U,AR,E)) :- atom(P),
	% All of the policy classes within the specified policy that
	% govern the entity E will be consulted.
	policy(P,Pr),
	findall(PC, (policy_class(P:Pr,PC), is_contained_in_pc(P:Pr,E,PC)), PCs),
	(   PCs == []
	->  !, fail
	;   foreach( member(Pc,PCs), policy_dpc_body(P:Pr,(U,AR,E),Pc) )
	).

% core test conditional version - policy_dpc/3
policy_dpc(P:Pr,(U,AR,E),CA) :- atom(P),
	% All of the policy classes within the specified policy that
	% govern the entity E will be consulted.
	policy(P,Pr),
	findall(PC, (policy_class(P:Pr,PC), is_contained_in_pc(P:Pr,E,PC)), PCs),
	(   PCs == []
	->  !, fail
	;   foreach( member(Pc,PCs), policy_dpc_body(P:Pr,(U,AR,E),Pc,CA) )
	).

% body - policy_dpc_body/3
policy_dpc_body(P:Pr,(U,AR,E),PC) :- % note c_associate/4
	c_associate(P:Pr,UA,ARs,AT),
	member(AR,ARs),
	is_contained_in(P:Pr,UA,PC),
	is_contained_in_pc(P:Pr,AT,PC),
	is_contained_in_ua(P:Pr,U,UA),
        is_contained_in_oa(P:Pr,E,AT),
	/* member(AR,ARs),*/ !.

% conditional version - policy_dpc_body/4
policy_dpc_body(P:Pr,(U,AR,E),PC,true) :- !, policy_dpc_body(P:Pr,(U,AR,E),PC).
policy_dpc_body(P:Pr,(U,AR,E),PC,CA) :- % note c_associate/5
	c_associate(P:Pr,UA,ARs,AT,CA),
	member(AR,ARs),
	is_contained_in(P:Pr,UA,PC),
	is_contained_in_pc(P:Pr,AT,PC),
	is_contained_in_ua(P:Pr,U,UA),
        is_contained_in_oa(P:Pr,E,AT),
	/* member(AR,ARs),*/ !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all derived privileges of the policy
policy_dps(P,DPS) :- atom(P), !, policy(P,Pr),
	findall(DP, policy_dpc(P:Pr,DP), DPs),
	sort(DPs,DPS).
policy_dps(P:Pr,DPS) :-
	findall(DP, policy_dpc(P:Pr,DP), DPs),
	sort(DPs,DPS).



%
% Used in the construction of variations of policy_dpc
% in following predicates P, Pr, PC must be valid
%
qualified_p_for_u(P:Pr,U) :- atom(U), user(P:Pr,U).
qualified_p_for_o(P:Pr,O) :- atom(O), object(P:Pr,O).
qualified_p_for_uo(P:Pr,U,O) :-
	qualified_p_for_u(P:Pr,U), qualified_p_for_o(P:Pr,O).

qualified_pc_for_u(P:Pr,PC,U) :- atom(U), is_contained_u_in_pc(P:Pr,U,PC).
qualified_pc_for_o(P:Pr,PC,O) :- atom(O), is_contained_o_in_pc(P:Pr,O,PC).
qualified_pc_for_uo(P:Pr,PC,U,O) :-
	qualified_pc_for_u(P:Pr,PC,U), qualified_pc_for_o(P:Pr,PC,O).

% E can be an object or an object_attribute in this predicate
% see is_contained_o_in_pc (a version specific to objects)
is_contained_in_pc(P:Pr, E, PC) :- atom(P), atom(Pr), atom(E), atom(PC), !,
	policy(P,Pr), policy_class(P:Pr,PC), object_oattribute(P:Pr,E),
	c_assign(P:Pr,OA,PC), object_oattribute(P:Pr,OA),
	(   E=OA
	->  !
	;   is_contained_in_oa1(P:Pr,E,OA), !
	).

% NEW for 'all' policy composition in policy_dpc and qualified_...
is_contained_u_in_ua(P:Pr, U, UA) :- c_assign(P:Pr,U,UA) . %, !.
is_contained_u_in_ua(P:Pr, U, UA) :-
	c_assign(P:Pr,UA1,UA), user_uattribute(P:Pr,UA1),
	is_contained_u_in_ua(P:Pr,U,UA1).

is_contained_o_in_pc(P:Pr, O, PC) :- atom(P), atom(O), atom(PC), !,
	policy(P,Pr), policy_class(P:Pr,PC), object(P:Pr,O),
	is_contained_in_pc(P:Pr,O,PC).

is_contained_u_in_pc(P:Pr, U, PC) :- atom(P), atom(U), atom(PC), !,
	policy(P,Pr), policy_class(P:Pr,PC), user(P:Pr,U),
	c_assign(P:Pr,UA,PC), user_uattribute(P:Pr,UA),
	(   U=UA
	->  !
	;   is_contained_u_in_ua(P:Pr, U, UA), !
	).

% Is a given object o assigned directly or indirectly to a
% given object attribute in the policy
%   is_contained_in_oa(Policy,Object,ObjectAttribute)
%
is_contained_in_oa(P:Pr, O, O) :- atom(P), atom(O),
	policy(P,Pr),
	object(P:Pr,O), !.
is_contained_in_oa(P:Pr, O, OA) :- atom(P), atom(OA), % policy(P,Pr),
	is_contained_in_oa1(P:Pr, O, OA).

is_contained_in_oa1(P:Pr, O, OA) :- atom(P), atom(OA), % defensive checks could be removed
	policy(P,Pr),
	c_assign(P:Pr,O,OA), object_oattribute(P:Pr,OA).
is_contained_in_oa1(P:Pr, O, OA) :- atom(P), atom(OA),
	policy(P,Pr),
	c_assign(P:Pr,OA1,OA),
	is_contained_in_oa1(P:Pr,O,OA1).

is_contained_in_ua(P:Pr, UorUA, UA) :- atom(P), atom(UA),
	policy(P,Pr),
	c_assign(P:Pr,UorUA,UA),
	(   element(P:Pr,user(UorUA)); element(P:Pr,user_attribute(UorUA)) ), !.
is_contained_in_ua(P:Pr, UorUA, UA) :- atom(P), atom(UA),
	policy(P,Pr),
	c_assign(P:Pr,UA1,UA),
	is_contained_in_ua(P:Pr,UorUA,UA1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% aoa(+P, +U, +PC, -AOA)
aoa(P, U, PC, AOA) :-
	atom(P), policy(P,PC), atom(U), element(P:PC,user(U)),
	atom(PC), element(P:PC,policy_class(PC)),
	findall(OA, aoa1(P, U, PC, OA), AOAu),
	findall(OA1, (member(OA2,AOAu),is_contained_in_oa(P:PC,OA1,OA2)), AOAv),
	append(AOAu,AOAv,AOAuv),
	sort(AOAuv,AOA).

aoa1(P, U, PC, OA) :-
	element(P:PC,user_attribute(UA)),
	is_contained_in_ua(P:PC, U, UA),
	c_associate(P:PC, UA, _Ops, OA),
	object_oattribute(P:PC,OA),
	is_contained_in(P:PC, UA, PC),
	is_contained_in(P:PC, OA, PC).

min_aoa(P, U, PC, MinAOA) :-
	aoa(P, U, PC, AOA),
	findall(OAm,
		(   member(OAm, AOA),
		    select(OAm, AOA, Aoa),
		    partition(is_contained_plus(P:PC,OAm),Aoa,[],_)
		), MinAOAu),
	sort(MinAOAu,MinAOA).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% aua(+P, +O, +PC, -AUO)
%
aua(P, O, PC, AUA) :-
	atom(P), policy(P,PC), atom(O), element(P:PC,object(O)),
	atom(PC), element(P:PC,policy_class(PC)),
	findall((UA,Ops), aua1(P, O, PC, UA, Ops), AUAu),
	findall((UA1,Ops2), (member((UA2,Ops2),AUAu),is_contained_u_in_ua(P:PC,UA1,UA2)), AUAv),
	append(AUAu,AUAv,AUAuv), sort(AUAuv,AUAs),
	aua_merge(AUAs,AUA).

aua1(P, O, PC, UA, Ops) :-
	object_oattribute(P:PC,OA),
	is_contained_in_oa(P:PC, O, OA),
	c_associate(P:PC, UA, Ops, OA),
	user_uattribute(P:PC,UA),
	is_contained_in(P:PC, UA, PC),
	is_contained_in(P:PC, OA, PC).

aua_merge([],[]).
aua_merge([(E,R)|ERs],Out) :-
	sort(R,Rs), aua_merge(ERs, (E,Rs), Out).

aua_merge([], (E,R), [(E,R)]).
aua_merge([(E,R)| E_Rs], (E1,R1), Out) :- E==E1, !,
	sort(R,Rs), ord_union(Rs,R1,R2),
	aua_merge( E_Rs, (E1,R2), Out ).
aua_merge([(E,R)| E_Rs], (E1,R1), [(E1,R1)|E_R_rest]) :- E\==E1, !,
	aua_merge( E_Rs, (E,R), E_R_rest ).

% aua_users/4 - no ar was specified; return users with associated ars
aua_users(P, O, PC, Users) :-
	aua(P, O, PC, AUA), % AUA = [(u_or_ua,[ars]),(u_or_ua,[ars]), ...]
	include( isa_user_aua(P:PC), AUA, Users).

isa_user_aua( PPC, (E,_) ) :- dpl:user(PPC,E).

% aua_users/3 - mode (ar) was specified; return only the list of users
aua_users(P, (AR, O), Users) :- atom(P), atom(AR), atom(O),
	aua(P, O, PC, AUA), % AUA = [(u_or_ua,[ars]),(u_or_ua,[ars]), ...]
	findall(E, (member((E,ARs),AUA), dpl:user(P:PC,E), memberchk(AR,ARs)), Users).

%  pdp:aua_users(P,O,_PC,M,Cond,Users) called from pqapi:users/3

% aua_users/6 when there's a condition
aua_users(P,O,_PC,AR,C,Users) :- var(AR), !, % no AR specified
	aua(P, O, PC, C, AUA), % AUA = [(u_or_ua,[ars]),(u_or_ua,[ars]), ...]
	include( isa_user_aua(P:PC), AUA, Users).

aua_users(P,O,_PC,AR,C,Users) :- !,
	aua(P, O, PC, C, AUA), % AUA = [(u_or_ua,[ars]),(u_or_ua,[ars]), ...]
	findall(E, (member((E,ARs),AUA), dpl:user(P:PC,E), memberchk(AR,ARs)), Users).

aua(P, O, PC, C, AUA) :-
	atom(P), policy(P,PC), atom(O), element(P:PC,object(O)),
	atom(PC), element(P:PC,policy_class(PC)),
	findall((UA,Ops), aua1(P, O, PC, UA, Ops, C), AUAu),
	findall((UA1,Ops2), (member((UA2,Ops2),AUAu),is_contained_u_in_ua(P:PC,UA1,UA2)), AUAv),
	append(AUAu,AUAv,AUAuv), sort(AUAuv,AUAs),
	aua_merge(AUAs,AUA).

aua1(P, O, PC, UA, Ops, C) :-
	object_oattribute(P:PC,OA),
	is_contained_in_oa(P:PC, O, OA),
	c_associate(P:PC, UA, Ops, OA, C),
	user_uattribute(P:PC,UA),
	is_contained_in(P:PC, UA, PC),
	is_contained_in(P:PC, OA, PC).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% los(LOS) - the Logical Object System under a policy
los(P, LOS) :- atom(P), policy(P,Pr),
	findall(user(U), (element(P:Pr,user(U)),c_assign(P:Pr,U,_)), Users),
	findall(object(O), (object(P:Pr,O),c_assign(P:Pr,O,_)), Objects),
	findall(user_attribute(UA),
		(element(P:Pr,user_attribute(UA)),c_assign(P:Pr,_,UA)), UAs1), sort(UAs1,UAs),
	findall(object_attribute(OA),
		(element(P:Pr,object_attribute(OA)),c_assign(P:Pr,_,OA)), OAs1), sort(OAs1,OAs),
	findall(policy_class(PC), (element(P:Pr,policy_class(PC)),c_assign(P:Pr,_,PC)), PCs1), sort(PCs1,PCs),
	findall(assign(X,XA), c_assign(P:Pr,X,XA), Assignments),
	findall(associate(UA,Rights,OA), c_associate(P:Pr,UA,Rights,OA), Associations),
	element(P:Pr,connector(C)),
	append([Users,Objects,UAs,OAs,PCs,Assignments,Associations,[connector(C)]],LOS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_los(+Policy, +User, -LOS_nodes, -LOS_edges)
user_los(P, U, LOSv, LOSe):- atom(P), policy(P,Pr),
	user_los_v(P:Pr,U,LOSv), user_los_e(P:Pr,LOSv,LOSe).

user_los_v(P, U, LOSv) :- % collect the nodes of the user LOS
	% gc(connector_name,CONN),
        CONN = 'PM',
	findall(PC, element(P,policy_class(PC)), PCs),
	findall(OA, (object_oattribute(P,OA), user_los_oa(P,U,OA)), OAs),
	append([[CONN],PCs,OAs], LOSv).

user_los_oa(P, U, OA) :- % OA is an object attribute of U in P
	element(P,policy_class(PC1)),
	is_contained_plus(P,OA,PC1),
	forall( element(P,policy_class(PC)),
		(\+ is_contained_plus(P,OA,PC)
		;   % user is authorised
		    element(P,user_attribute(UA)),
		    object_oattribute(P,OAm),
		    is_contained_plus(P,U,UA),
		    c_associate(P,UA,_OPS,OAm),
		    is_contained_plus(P,UA,PC),
		    is_contained_plus(P,OAm,PC),
		    is_contained_star(P,OA,OAm)
		) ), !.

user_los_e(P,LOSv,LOSe) :- % construct the edges of the user LOS
	%gc(connector_name,CONN),
        CONN = 'PM',
	findall(PC, (element(P,policy_class(PC)),member(PC,LOSv)), LOSpcs),
	findall(OA, (object_oattribute(P,OA),member(OA,LOSv)), LOSoas),
	findall((PC,CONN), member(PC,LOSpcs), PC_CONNs),
	findall((OA,PC),
		(   member(OA,LOSoas), member(PC,LOSpcs),
		    is_contained_plus(P,OA,PC),
		    \+ (member(OAp,LOSoas),
			is_contained_plus(P,OA,OAp),
			is_contained_plus(P,OAp,PC))
		), OA_PCs),
	findall((OA1,OA2),
		(   member(OA1,LOSoas), member(OA2,LOSoas),
		    is_contained_plus(P,OA1,OA2),
		    \+ (member(OAp,LOSoas),
			is_contained_plus(P,OA1,OAp),
			is_contained_plus(P,OAp,OA2))
		), OA_OAs),
	append([PC_CONNs,OA_PCs,OA_OAs], LOSe).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% oa is visible by u in PC
is_visible(P, OA, U, PC) :-
	atom(P), policy(P,_), atom(OA), object_oattribute(P,OA),
	atom(U), element(P,user(U)), atom(PC), element(P,policy_class(PC)),
	min_aoa(P, U, PC, MinAOA),
	member(OAm,MinAOA), is_contained_star(P, OA, OAm),
	findall(PCp, (element(P,policy_class(PCp)),PCp\==PC), PCps),
	is_vis_chk(P, OA, U, PCps).

is_vis_chk(_, _, _, []).
is_vis_chk(P, OA, U, [PCp|PCps]) :-
	\+ is_contained_plus(P, OA, PCp),
	is_vis_chk(P, OA, U, PCps).
is_vis_chk(P, OA, U, [PCp|PCps]) :-
	min_aoa(P, U, PCp, MinAOAp),
	is_vis_chk2(P, OA, MinAOAp),
	is_vis_chk(P, OA, U, PCps).

is_vis_chk2(_,_,[]) :- !, fail.
is_vis_chk2(P, OA,[OAmp|_OAmps]) :-
	is_contained_in(P, OA, OAmp), !.
is_vis_chk2(P, OA,[_,OAmps]) :-
	is_vis_chk2(P, OA, OAmps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_contained_star(+P, +E1, +E2)
is_contained_star(P:Pr,E,E) :- atom(P), policy(P,Pr), atom(E), object_oattribute(P:Pr,E), !.
is_contained_star(P:Pr,E1,E2) :- is_contained_plus(P:Pr,E1,E2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_contained_plus(+P, +E1, +E2)
is_contained_plus(P:Pr,E1,E2) :- is_contained_in(P:Pr,E1,E2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_contained_in(+P, +E1, +E2)
is_contained_in(P:Pr, E1, E2) :-
	atom(P), policy(P,Pr), atom(E1), atom(E2),
	c_assign(P:Pr,E1,E2), !.
is_contained_in(P:Pr, E1, E2) :-
	atom(P), policy(P,Pr), atom(E1), atom(E2),
	c_assign(P:Pr, E1p, E2),
	is_contained_in(P:Pr, E1, E1p).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

access(P,U,M,O) :-
	is_contained_in_ua(P,U,UA), % pred currently requires UA is atom
	c_associate(P,UA,Xs,OA),
	is_contained_in_oa(P,O,OA),
	memberchk(M,Xs),
	true.

%
policy_dp1(P:Pr,(_U,_AR,O),PC) :-
	\+ is_contained_o_in_pc(P:Pr, O, PC), !, fail.
policy_dp1(P:Pr,(U,AR,O),_PC) :- atom(P),
	policy(P,Pr),
	c_associate(P:Pr,UA,ARs,OA),
	member(AR,ARs),
	is_contained_in_ua(P:Pr,U,UA),
	is_contained_in_oa(P:Pr,O,OA).
%
policy_dp(P:Pr,(U,AR,O)) :- atom(P),
	policy(P,Pr),
	c_associate(P:Pr,UA,ARs,OA),
	member(AR,ARs),
	is_contained_in_ua(P:Pr,U,UA),
	is_contained_in_oa(P:Pr,O,OA).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_los_objects(P, U, LOSobjs) - objects accessible to user in P
user_los_objects(P, U, LOSobjs) :- atom(P), atom(U),
	policy_dps(P,DPS),
	findall((U,AR,O), member((U,AR,O),DPS), UDPS),
	u_ars_o_view(UDPS,LOSobjs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
u_ars_o_view([],[]).
u_ars_o_view([X|Xs],[(U,ARs,O)|LOS]) :-
	X = (U,_,O),
	findall(AR, member((U,AR,O),[X|Xs]), ARs),
	findall((U,Y,O), member((U,Y,O),Xs), DXs),
	ord_subtract(Xs,DXs,NXs),
	u_ars_o_view(NXs, LOS),
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_cap(P, U, Cap) - Cap is a current session capability of user in P
user_cap(P, U, (Op, O)) :-
	policy_dps(P,DPS),
	member((U, Op, O), DPS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
